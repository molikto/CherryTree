package model.operation

import model._
import Type.Type
import com.softwaremill.quicklens._
import model.range.IntRange

import scala.util.Random


abstract sealed class Node extends OperationOnModal[data.Node, mode.Node] {
  override type This = Node
}

object Node extends OperationObject[data.Node, operation.Node] {
  def deleteRanges(aa: Seq[range.Node]): Seq[Node] = {
    val ddd = aa.foldLeft[(range.Node => Option[range.Node], Seq[range.Node])]((a => Some(a), Seq.empty)) { (pair, a) =>
      val r2 = pair._1(a)
      val func = if (r2.isEmpty) pair._1 else (j: range.Node) => { pair._1(j).flatMap(kk => r2.get.transformDeletingRangeAfterDeleted(kk)) }
      (func, pair._2 ++ r2)
    }._2
    merge(ddd.map(a => operation.Node.Delete(a)))
  }

  def rich(c: cursor.Node, a: operation.Rich): Node = operation.Node.Content(c, operation.Content.Rich(a))
  def rich(c: cursor.Node, a: Seq[operation.Rich]): Seq[Node] = a.map(a => rich(c, a))

  def transform(nodeAfter: data.Node, ns: Seq[Node], a: (mode.Node, Boolean)): (mode.Node, Boolean) = {
    val (c, j) = ns.foldLeft(a) { (a, n) => n.transform(a) }
    fixTransformMode(nodeAfter, c, j)
  }

  // LATER transform code is written very badly with various hacks
  private def fixTransformMode(a: data.Node, m: mode.Node, isBad: Boolean): (mode.Node, Boolean) = {
    val mode = if (isBad) {
      m match {
        case model.mode.Node.Content(cur, c) =>
          val tt = c match {
            case null => a(cur).content.defaultNormalMode()
            case model.mode.Content.RichInsert(ins) => model.mode.Content.RichNormal(a(cur).rich.after(ins).range)
            case t => t
          }
          model.mode.Node.Content(cur, tt)
        case j => j
      }
    } else {
      m
    }
    (mode, isBad)
  }

  case class Content(at: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content(a.content)))
    }

    override def transform(a: MM): MODE = a match {
      case c: mode.Node.Content if c.node == at =>
        if (c.a != null) {
          val (m, f) = content.transform(c.a)
          (c.copy(a = m), f)
        } else {
          MODE(a)
        }
      case _ => MODE(a)
    }

    override def reverse(d: data.Node): Node = copy(content = content.reverse(d(at).content))

    override def mergeForUndoer(before: Node): Option[Node] = before match {
      case Content(at0, c0) if at0 == at => content.mergeForUndoer(c0).map(a => Content(at, a))
      case _ => None
    }

    override def merge(before: Any): Option[Node] = before match {
      case Content(at0, c0) if at0 == at => content.merge(c0).map(a => Content(at, a))
      case _ => None
    }

    override def isEmpty: Boolean = content.isEmpty
  }
  case class Replace(at: cursor.Node, content: data.Content) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content))
    }

    override def toString: String = s"Replace($at)"

    override def transform(a: MM): MODE = a match {
      case c: mode.Node.Content if c.node == at => (mode.Node.Content(at, content.defaultNormalMode()), false)
      case _ => MODE(a)
    }

    override def reverse(d: data.Node): Node = Replace(at, d(at).content)

    override def merge(before: Any): Option[Node] = before match {
      case Replace(a, _) if a == at => Some(this)
      case Content(a, _) if a == at => Some(this)
      case _ => None
    }

    override def isEmpty: Boolean = false
  }
  case class Insert(at: cursor.Node, childs: Seq[data.Node]) extends Node {
    override def ty: Type = Type.Add
    override def apply(d: data.Node): data.Node = d.insert(at, childs)


    override def toString: String = s"Insert($at, ${childs.size})"

    override def transform(a: MM): MODE = a match {
      case c: mode.Node.Content =>
        MODE(c.modify(_.node).using(a => cursor.Node.transformAfterInserted(at, childs.size, a)))
      case mode.Node.Visual(fix, move) =>
        MODE(mode.Node.Visual(
          cursor.Node.transformAfterInserted(at, childs.size, fix),
          cursor.Node.transformAfterInserted(at, childs.size, move)))
    }
    override def reverse(d: data.Node): Node = Delete(range.Node(at, len = childs.size))

    override def merge(before: Any): Option[Node] = before match {
      case Insert(aa, css) =>
        val newRange = range.Node(aa, len = css.length)
        if (newRange.sameParent(at) && (newRange.contains(at) || newRange.until == at)) {
          if (aa == at) {
            Some(Insert(aa, childs ++ css))
          } else {
            Some(Insert(aa, css.patch(at.last - aa.last, childs, 0)))
          }
        } else {
          None
        }
      case _ => None
    }

    override def isEmpty: Boolean = childs.isEmpty
  }
  case class Delete(r: range.Node) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = d.delete(r)

    override def transform(a: MM): MODE = a match {
      case c@mode.Node.Content(node, _) => r.transformAfterDeleted(node) match {
        case Some(k) => MODE(c.copy(node = k))
        case None => MODE(mode.Node.Content(r.parent, null), false)
      }
      case mode.Node.Visual(fix, move) =>
        (r.transformAfterDeleted(fix), r.transformAfterDeleted(move)) match {
          case (Some(ff), Some(mm)) => MODE(mode.Node.Visual(ff, mm))
          case (Some(ff), None) => MODE(mode.Node.Content(ff, null), false)
          case (None, Some(ff))  =>  MODE(mode.Node.Content(ff, null), false)
          case (None, None) => MODE(mode.Node.Content(r.parent, null), false)
        }
    }

    override def reverse(d: data.Node): Node = Insert(r.start, d(r.parent).apply(r.childs))

    override def merge(before: Any): Option[Node] = before match {
      case Node.Content(at, _) if r.contains(at) => Some(this)
      case Node.Replace(at, _) if r.contains(at) => Some(this)
      case Node.Insert(at, childs) if r.contains(at.dropRight(1)) => Some(this)
      case Node.Delete(rr) =>
        if (r.contains(rr.parent)) {
          Some(this)
        } else if (r.parent == rr.parent && r.childs.containsInsertion(rr.childs.start)) {
          Some(Delete(range.Node(r.parent, IntRange(r.childs.start, r.childs.until + rr.childs.size))))
        } else {
          None
        }
      case m@Node.Move(rr, to) if r.contains(rr.transformNodeAfterMoved(to, to).dropRight(1)) => merge(Delete(rr))
      case _ => None
    }

    override def isEmpty: Boolean = r.isEmpty
  }
  case class Move(r: range.Node, to: cursor.Node) extends Node {
    assert(!r.contains(to))
    override def ty: Type = Type.Structural
    override def apply(d: data.Node): data.Node = d.move(r, to)

    override def transform(a: MM): MODE = a match {
      case mode.Node.Visual(fix, move) => MODE(mode.Node.Visual(r.transformNodeAfterMoved(to, fix), r.transformNodeAfterMoved(to, move)))
      case mode.Node.Content(node, b) => MODE(mode.Node.Content(r.transformNodeAfterMoved(to, node), b))
    }

    def reverse = operation.Node.Move(
        range.Node(cursor.Node.moveBy(r.transformNodeAfterMoved(to, to), -r.size), r.size),
        r.transformNodeAfterMoved(to, r.until)
      )

    override def reverse(d: data.Node): Node = reverse

    override def merge(before: Any): Option[Node] = None

    override def isEmpty: Boolean = r.isEmpty || r.until == to
  }

  override val pickler: Pickler[Node] = new Pickler[Node] {
    override def pickle(obj: Node)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Content(c, content) =>
          writeInt(0)
          writeIntArray(c.toArray)
          operation.Content.pickler.pickle(content)
        case Replace(c, content) =>
          writeInt(1)
          writeIntArray(c.toArray)
          data.Content.pickler.pickle(content)
        case Insert(c, childs) =>
          writeInt(2)
          writeIntArray(c.toArray)
          writeInt(childs.size)
          childs.foreach(a => data.Node.pickler.pickle(a))
        case Delete(r) =>
          writeInt(3)
          range.Node.pickler.pickle(r)
        case Move(r, a) =>
          writeInt(4)
          range.Node.pickler.pickle(r)
          writeIntArray(a.toArray)
      }
    }

    override def unpickle(implicit state: UnpickleState): Node = {
      import state.dec._
      readInt match {
        case 0 =>
          Content(readIntArray, operation.Content.pickler.unpickle)
        case 1 =>
          Replace(readIntArray, data.Content.pickler.unpickle)
        case 2 =>
          Insert(readIntArray, (0 until readInt).map(_ => data.Node.pickler.unpickle))
        case 3 =>
          Delete(range.Node.pickler.unpickle)
        case 4 =>
          Move(range.Node.pickler.unpickle, readIntArray)
      }
    }
  }

  def randomCursorAndNode(d: data.Node, r: Random): (cursor.Node, data.Node) = {
    if (d.childs.isEmpty || r.nextInt(3) == 0) {
      (cursor.Node.root, d)
    } else {
      val c = r.nextInt(d.childs.size)
      val (aa, bb) = randomCursorAndNode(d.childs(c), r)
      (c +: aa, bb)
    }
  }

  override def random(d: data.Node, r: Random): Node = {
    def doInsert(): Node = {
      val (c, n) = randomCursorAndNode(d, r)
      val p = r.nextInt(n.childs.size + 1)
      Insert(c :+ p, (0 until r.nextInt(3)).map(_ => data.Node.random(r)))
    }
    r.nextInt(5) match {
      case 0 =>
        val (c, n) = randomCursorAndNode(d, r)
        Content(c, operation.Content.random(n.content, r))
      case 1 =>
        val (c, _) = randomCursorAndNode(d, r)
        Replace(c, data.Content.random(r))
      case 2 =>
        val (c, _) = randomCursorAndNode(d, r)
        if (c != cursor.Node.root) {
          var ran = range.Node(c)
          if (r.nextBoolean() && ran.childs.start > 2) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 3))
          } else if (r.nextBoolean() && ran.childs.start > 1) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 2))
          } else if (r.nextBoolean() && ran.childs.start > 0) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 1))
          }
          Delete(ran)
        } else {
          doInsert()
        }
      case 4 =>
        val (c, _) = randomCursorAndNode(d, r)
        val (c2, _) = randomCursorAndNode(d, r)
        if (c != cursor.Node.root && c2 != cursor.Node.root) {
          var ran = range.Node(c)
          if (r.nextBoolean() && ran.childs.start > 2) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 3))
          } else if (r.nextBoolean() && ran.childs.start > 1) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 2))
          } else if (r.nextBoolean() && ran.childs.start > 0) {
            ran = range.Node(ran.parent, ran.childs.modify(_.start).using(_ - 1))
          }
          if (!ran.contains(c2) && ran.until != c2) {
            Move(ran, c2)
          } else {
            doInsert()
          }
        } else {
          doInsert()
        }
      case 3 =>
        doInsert()
    }
  }
}
