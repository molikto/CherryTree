package shared.data

import boopickle.CompositePickler
import boopickle.Default._
import shared.data
import shared.data.Node.{Content => C}
import shared.data.{Node => N}
import com.softwaremill.quicklens._


trait ChangeImplicits {

  implicit val changeNodePickler: CompositePickler[Change.Node] =
    compositePickler[Change.Node]
      .addConcreteType[Change.Node.Delete]
      .addConcreteType[Change.Node.Insert]

  implicit val changeContentPickler: CompositePickler[Change.Content] =
    compositePickler[Change.Content]
      .addConcreteType[Change.Content.Insert]
      .addConcreteType[Change.Content.Delete]

  implicit val changePickler: CompositePickler[Change] =
    compositePickler[Change]
      .join[Change.Node](changeNodePickler)
      .join[Change.Content](changeContentPickler)
}

sealed trait Change {
  /**
    * @return None if this is destructive, even partially
    */
  def map(ref: N.Ref): Option[N.Ref]

  /**
    * @return None if this is destructive, even partially
    */
  def map(ref: N.PointRef): Option[N.PointRef]

  /**
    * @return None if this is destructive, even partially
    */
  def map(ref: N.SegmentRef): Option[N.SegmentRef]

  /**
    * @return None if this is destructive, even partially
    */
  def rebase(loser: Change): Rebased[Change]

  def rebasePair(loser: Change): (Rebased[(Change, Change)]) = {
    (rebase(loser), loser.rebase(this)) match {
      case (Rebased(b, sb), Rebased(a, sa)) if sa.isEmpty && sb.isEmpty =>
        Rebased.Free((a, b))
      case (Rebased(b, sb), Rebased(a, sa)) if sb == Set(RebaseConflict.WinnerDeletesLoser) && sa == Set(RebaseConflict.LoserDeletesWinner) =>
        Rebased.WinnerDeletesLoser((a, b))
      case (Rebased(b, sb), Rebased(a, sa)) if sb == Set(RebaseConflict.LoserDeletesWinner) && sa == Set(RebaseConflict.WinnerDeletesLoser) =>
        Rebased.LoserDeletesWinner((a, b))
      case _ => throw new IllegalStateException("You should override this!!!")
    }
  }
}

object Change {

  case object Id extends Change {
    override def map(ref: N.Ref) = Some(ref)

    override def map(ref: N.PointRef) = Some(ref)

    override def map(ref: N.SegmentRef) = Some(ref)

    override def rebase(loser: Change): Rebased[Change] = Rebased.Free(loser)
  }

  sealed trait Node extends Change

  object Node {

    case class Delete(position: N.Ref) extends Node() {
      assert(position != N.Ref.root)

      override def map(ref: N.Ref): Option[N.Ref] =
        N.Ref.transformAfterDeleted(position, ref)

      override def map(ref: N.PointRef): Option[N.PointRef] =
        map(ref.node).map(c => N.PointRef(c, ref.content))

      override def map(ref: N.SegmentRef): Option[N.SegmentRef] =
        map(ref.node).map(c => N.SegmentRef(c, ref.content))

      override def rebase(loser: Change): Rebased[Change] =
        loser match {
          case d: Node.Delete =>
            // if we cannot find the deleted node, then we are fine!
            Rebased.Free(map(d.position).map(c => Delete(c)).getOrElse(Id))
          case i: Node.Insert =>
            if (i.position == position) {
              Rebased.Free(i)
            } else {
              map(i.position).map(c => Rebased.Free[Change](Insert(c, i.node)))
                .getOrElse(Rebased.WinnerDeletesLoser(Id))
            }
          case d: Content.Delete =>
            // if we cannot find the deleted segment, we are fine!
            Rebased.Free(map(d.segment.node)
              .map(c => d.modify(_.segment.node).using(_ => c))
              .getOrElse(Id))
          case i: Content.Insert =>
            map(i.point.node).map(c => Rebased.Free[Change](i.modify(_.point.node).using(_ => c)))
              .getOrElse(Rebased.WinnerDeletesLoser(Id))
          case Id => Rebased.Free(Id)
        }
    }

    case class Insert(position: N.Ref, node: N) extends Node() {
      assert(position != N.Ref.root)

      override def map(ref: N.Ref): Option[N.Ref] =
        Some(N.Ref.transformAfterInserted(position, ref))

      override def map(ref: N.PointRef): Option[N.PointRef] =
        map(ref.node).map(c => N.PointRef(c, ref.content))

      override def map(ref: N.SegmentRef): Option[N.SegmentRef] =
        map(ref.node).map(c => N.SegmentRef(c, ref.content))

      override def rebase(loser: Change): Rebased[Change] = {
        loser match {
          case d: Node.Delete =>
            if (position.childOf(d.position)) {
              Rebased.LoserDeletesWinner(d)
            } else {
              Rebased.Free(map(d.position).map(c => Delete(c)).get)
            }
          case i: Node.Insert =>
            if (position == i.position) {
              Rebased.Asymmetry(i)
            } else {
              Rebased.Free(map(i.position).map(c => Insert(c, i.node)).get)
            }
          case d: Content.Delete =>
            Rebased.Free(map(d.segment.node).map(c => d.modify(_.segment.node).using(_ => c)).get)
          case i: Content.Insert =>
            Rebased.Free(map(i.point.node).map(c => i.modify(_.point.node).using(_ => c)).get)
          case Id => Rebased.Free(Id)
        }
      }

      override def rebasePair(loser: Change): Rebased[(Change, Change)] = {
        loser match {
          case i: Node.Insert if position == i.position =>
            Rebased((Insert(position.next, node), i), Set(RebaseConflict.Asymmetry))
          case _ => super.rebasePair(loser)
        }
      }
    }

  }

  sealed trait Content extends Change

  object Content {

    case class Insert(point: N.PointRef, content: C) extends Content() {
      assert(content.nonEmpty)

      override def map(ref: N.Ref) = Some(ref)

      override def map(ref: N.PointRef): Some[N.PointRef] = {
        val res =
          if (point.node == ref.node)
            ref.copy(content = C.transformAfterInserted(point.content, content.length, ref.content))
          else ref
        Some(res)
      }

      override def map(ref: N.SegmentRef): Option[N.SegmentRef] = {
        val res =
          if (point.node == ref.node)
            ref.copy(content = C.SegmentRef(
              C.transformAfterInserted(point.content, content.length, ref.content.from),
              C.transformAfterInserted(point.content, content.length, ref.content.to)
            ))
          else ref
        Some(res)
      }

      override def rebase(loser: Change): Rebased[Change] = {
        loser match {
          case d: Node.Delete =>
            if (point.node.eqOrChildOf(d.position)) {
              Rebased.LoserDeletesWinner(d)
            } else {
              Rebased.Free(d)
            }
          case i: Node.Insert => Rebased.Free(i)
          case d: Content.Delete =>
            if (d.segment.node == point.node) {
              if (d.segment.content.leftOpenContains(point.content)) {
                Rebased.LoserDeletesWinner(
                  d.modify(_.segment.content.to).using(_ + content.length))
              } else {
                Rebased.Free(map(d.segment).map(s => d.copy(segment = s)).get)
              }
            } else {
              Rebased.Free(d)
            }
          case i: Content.Insert =>
            if (i.point.node == point.node) {
              if (point.content == i.point.content) {
                Rebased.Asymmetry(i)
              } else {
                Rebased.Free(map(i.point).map(s => i.copy(point = s)).get)
              }
            } else {
              Rebased.Free(i)
            }
          case Id => Rebased.Free(Id)
        }
      }

      override def rebasePair(loser: Change): Rebased[(Change, Change)] = {
        loser match {
          case i: Content.Insert if point == i.point =>
            // no change our side!!!
            Rebased((this.modify(_.point.content).using(_ + i.content.size), i), Set(RebaseConflict.Asymmetry))
          case _ => super.rebasePair(loser)
        }
      }
    }

    case class Delete(segment: N.SegmentRef) extends Content() {
      override def map(ref: N.Ref) = Some(ref)

      override def map(ref: N.PointRef): Option[N.PointRef] = {
        if (segment.node == ref.node)
          C.transformAfterDeleted(segment.content, ref.content).map(s => ref.copy(content = s))
        else Some(ref)
      }

      override def map(ref: N.SegmentRef): Option[N.SegmentRef] = {
        if (segment.node == ref.node)
          C.transformAfterDeleted(segment.content, ref.content).map(s => ref.copy(content = s))
        else Some(ref)
      }

      override def rebase(loser: Change): Rebased[Change] =
        loser match {
          case d: Node.Delete => Rebased.Free(d)
          case i: Node.Insert => Rebased.Free(i)
          case d: Content.Delete =>
            if (segment.node == d.segment.node)
              Rebased.Free(C.transformDeletingSegmentAfterDeleted(segment.content, d.segment.content).map(s =>
                d.copy(segment = d.segment.copy(content = s))).getOrElse(Id))
            else Rebased.Free(d)
          case i: Content.Insert =>
            if (i.point == segment.from) {
              Rebased.Free(i)
            } else {
              map(i.point).map(it => Rebased.Free(i.copy(point = it)).asInstanceOf[Rebased[Change]])
                .getOrElse(Rebased.WinnerDeletesLoser(Id))
            }
          case Id => Rebased.Free(Id)
        }
    }

  }


  /**
    *
    * @param root
    * @param changes change is properly based!
    */
  def apply(root: data.Node, changes: Seq[Change]): (data.Node, Seq[Change]) = {
    changes.foldLeft((root, Seq.empty[Change])) { (pair, c) =>
      val (nodep, r) = apply(pair._1, c)
      (nodep, r +: pair._2)
    }
  }
  /**
    * @return return a pair of new root, and a reverse change
    */
  def apply(root: data.Node, change: Change): (data.Node, Change) = {
    change match {
      case i: Node.Insert =>
        (root.insert(i.position, i.node), Change.Node.Delete(i.position))
      case d: Node.Delete =>
        val (node, content) = root.delete(d.position)
        (node, Change.Node.Insert(d.position, content))
      case c: Content.Delete =>
        val (node, content) = root.delete(c.segment)
        (node, Change.Content.Insert(c.segment.from, content))
      case c: Content.Insert =>
        (root.insert(c.point, c.content), Change.Content.Delete(c.point.to(c.content.length)))
      case Id =>
        (root, Id)
    }
  }


  def rebaseLine(winner: Change, loser: Seq[Change]): Rebased[(Change, Seq[Change])] = {
    loser.foldLeft(Rebased.Free(winner, Seq.empty[Change])) { (pair, ll) =>
      pair match {
        case Rebased((wi, lp), t) =>
          val Rebased((wi0, lp0), t0) = wi.rebasePair(ll)
          Rebased((wi0, lp :+ lp0), t ++ t0)
      }
    }
  }

  def rebaseLine(winner: Seq[Change], loser: Change): Rebased[(Seq[Change], Change)] = {
    winner.foldLeft(Rebased.Free(Seq.empty[Change], loser)) { (pair, ww) =>
      pair match {
        case Rebased((wp, li), t) =>
          val Rebased((wp0, li0), t0) = ww.rebasePair(li)
          Rebased((wp :+ wp0, li0), t ++ t0)
      }
    }
  }

  /**
    * l  /\  w
    * w' / l'
    *
    * @param winner winner
    * @param loser loser
    * @return (wp, lp)
    */
  def rebaseSquare(winner: Seq[Change], loser: Seq[Change]): Rebased[(Seq[Change], Seq[Change])] = {
    loser.foldLeft(Rebased.Free(winner, Seq.empty[Change])) { (pair, ll) =>
      pair match {
        case Rebased((wi, lp), t) =>
          val Rebased((wi0, lp0), t0) = rebaseLine(wi, ll)
          Rebased((wi0, lp :+ lp0), t ++ t0)
      }
    }
  }
}

