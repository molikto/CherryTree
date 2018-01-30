package shared.data

import boopickle.CompositePickler
import boopickle.Default._
import shared.data
import shared.data.Node.{Content => C}
import shared.data.{Node => N}

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
  def mapOption(ref: N.Ref): Option[N.Ref]

  /**
    * @return None if this is destructive, even partially
    */
  def mapOption(ref: N.PointRef): Option[N.PointRef]

  /**
    * @return None if this is destructive, even partially
    */
  def mapOption(ref: N.SegmentRef): Option[N.SegmentRef]

  /**
    * @return None if this is destructive, even partially
    */
  def rebaseOption(o: Change): Option[Change]
}

object Change {

  case object Id extends Change {
    override def mapOption(ref: N.Ref) = Some(ref)

    override def mapOption(ref: N.PointRef) = Some(ref)

    override def mapOption(ref: N.SegmentRef) = Some(ref)

    override def rebaseOption(o: Change) = Some(o)
  }

  sealed trait Node extends Change

  object Node {

    case class Delete(position: N.Ref) extends Node() {
      assert(position != N.Ref.root)

      override def mapOption(ref: N.Ref): Option[N.Ref] =
        N.Ref.transformAfterDeleted(position, ref)

      override def mapOption(ref: N.PointRef): Option[N.PointRef] =
        mapOption(ref.node).map(c => N.PointRef(c, ref.content))

      override def mapOption(ref: N.SegmentRef): Option[N.SegmentRef] =
        mapOption(ref.node).map(c => N.SegmentRef(c, ref.content))

      override def rebaseOption(o: Change): Option[Change] =
        o match {
          case d: Node.Delete =>
            // if we cannot find the deleted node, then we are fine!
            mapOption(d.position).map(c => Delete(c)).orElse(Some(Id))
          case i: Node.Insert =>
            if (i.position == position) {
              Some(i)
            } else {
              mapOption(i.position).map(c => Insert(c, i.node))
            }
          case d: Content.Delete =>
            // if we cannot find the deleted segment, we are fine!
            mapOption(d.segment.node)
              .map(c => d.copy(segment = d.segment.copy(node = c)))
              .orElse(Some(Id))
          case i: Content.Insert =>
            mapOption(i.point.node).map(c => i.copy(point = i.point.copy(node = c)))
          case Id => Some(Id)
        }
    }

    case class Insert(position: N.Ref, node: N) extends Node() {
      assert(position != N.Ref.root)

      override def mapOption(ref: N.Ref): Option[N.Ref] =
        Some(N.Ref.transformAfterInserted(position, ref))

      override def mapOption(ref: N.PointRef): Option[N.PointRef] =
        mapOption(ref.node).map(c => N.PointRef(c, ref.content))

      override def mapOption(ref: N.SegmentRef): Option[N.SegmentRef] =
        mapOption(ref.node).map(c => N.SegmentRef(c, ref.content))

      override def rebaseOption(o: Change): Option[Change] =
        o match {
          case d: Node.Delete =>
            if (position.childOf(d.position)) {
              None
            } else {
              mapOption(d.position).map(c => Delete(c))
            }
          case i: Node.Insert =>
            // weak conflict
            mapOption(i.position).map(c => Insert(c, i.node))
          case d: Content.Delete =>
            mapOption(d.segment.node).map(c => d.copy(segment = d.segment.copy(node = c)))
          case i: Content.Insert =>
            mapOption(i.point.node).map(c => i.copy(point = i.point.copy(node = c)))
          case Id => Some(Id)
        }
    }

  }

  sealed trait Content extends Change

  object Content {

    case class Insert(point: N.PointRef, content: C) extends Content() {
      assert(content.nonEmpty)

      override def mapOption(ref: N.Ref) = Some(ref)

      override def mapOption(ref: N.PointRef): Some[N.PointRef] = {
        val res =
          if (point.node == ref.node)
            ref.copy(content = C.transformAfterInserted(point.content, content.length, ref.content))
          else ref
        Some(res)
      }

      override def mapOption(ref: N.SegmentRef): Option[N.SegmentRef] = {
        val res =
          if (point.node == ref.node)
            ref.copy(content = C.SegmentRef(
              C.transformAfterInserted(point.content, content.length, ref.content.from),
              C.transformAfterInserted(point.content, content.length, ref.content.to)
            ))
          else ref
        Some(res)
      }

      override def rebaseOption(o: Change): Option[Change] =
        o match {
          case d: Node.Delete =>
            if (point.node.eqOrChildOf(d.position)) {
              None
            } else {
              Some(d)
            }
          case i: Node.Insert => Some(i)
          case d: Content.Delete =>
            if (d.segment.node == point.node) {
              if (d.segment.content.contains(point.content)) {
                None
              } else {
                mapOption(d.segment).map(s => d.copy(segment = s))
              }
            } else {
              Some(d)
            }
          case i: Content.Insert =>
            if (i.point.node == point.node) {
              mapOption(i.point).map(s => i.copy(point = s))
            } else {
              Some(i)
            }
          case Id => Some(Id)
        }
    }

    case class Delete(segment: N.SegmentRef) extends Content() {
      override def mapOption(ref: N.Ref) = Some(ref)

      override def mapOption(ref: N.PointRef): Option[N.PointRef] = {
        if (segment.node == ref.node)
          C.transformAfterDeleted(segment.content, ref.content).map(s => ref.copy(content = s))
        else Some(ref)
      }

      override def mapOption(ref: N.SegmentRef): Option[N.SegmentRef] = {
        if (segment.node == ref.node)
          C.transformAfterDeleted(segment.content, ref.content).map(s => ref.copy(content = s))
        else Some(ref)
      }

      override def rebaseOption(o: Change): Option[Change] =
        o match {
          case d: Node.Delete => Some(d)
          case i: Node.Insert => Some(i)
          case d: Content.Delete =>
            if (segment.node == d.segment.node)
              C.transformDeletingSegmentAfterDeleted(segment.content, d.segment.content).map(s =>
                d.copy(segment = d.segment.copy(content = s))).orElse(Some(Id))
            else Some(d)
          case i: Content.Insert =>
            mapOption(i.point).map(it => i.copy(point = it))
          case Id => Some(Id)
        }
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


  /**
    *
    * @return a' b'
    */
  def rebaseOptionPair(a: Change, b: Change): Option[(Change, Change)] = {
    (a.rebaseOption(b), b.rebaseOption(a)) match {
      case (Some(bp), Some(ap)) =>
        Some(ap, bp)
      case (None, None) => None
      case (aa, bb) => throw new IllegalArgumentException(s"Not matching rebase b': $aa, a': $bb")
    }
  }

  def rebaseOptionLine(a: Change, b: Seq[Change]): Option[(Change, Seq[Change])] = {
    b.foldLeft(Option((a, Seq.empty[Change]))) { (pair, bb) =>
      pair.flatMap {
        case (lp, bp) =>
          rebaseOptionPair(lp, bb).map {
            case (lpp, bbp) =>
              (lpp, bp :+ bbp)
          }
      }
    }
  }

  /**
    * l  /\  w
    * w' / l'
    *
    * @param w winner
    * @param l loser
    * @return (wp, lp)
    */
  def rebaseOption(w: Seq[Change], l: Seq[Change]): Option[(Seq[Change], Seq[Change])] = {
    l.foldLeft(Option((w, Seq.empty[Change]))) { (pair, ll) =>
      pair.flatMap {
        case (wpp, lpb) =>
          rebaseOptionLine(ll, wpp).map {
            case (llp, wppp) =>
              (wppp, lpb :+ llp)
          }
      }
    }
  }
}

}

