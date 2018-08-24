package model.operation

import model.{data, _}
import Type.Type
import com.softwaremill.quicklens._
import doc.DocState
import model.data.NodeTag
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


abstract sealed class Node extends Operation[data.Node] {
  override type This = Node

  def apply(a: DocState, enableModal: Boolean): DocState
}

object Node extends OperationObject[data.Node, operation.Node] {


  def apply(transforms: transaction.Node, a: DocState, enableModal: Boolean): (DocState, Seq[(DocState, operation.Node)]) = {
    var aa = a
    val bf = new ArrayBuffer[DocState]()
    bf.append(a)
    for (t <- transforms) {
      aa = t(aa, enableModal)
      bf.append(aa)
    }
    val last = bf.last
    val from = bf.dropRight(1).zip(transforms)
    (last, from)
  }

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

  object AttributeChange {
    def apply[T](at: cursor.Node, tag: NodeTag[T], to: Option[T]): AttributeChange =
      AttributeChange(at, tag.name, to.map(tag.serialize).getOrElse(""))
  }
  case class AttributeChange(at: cursor.Node, tag: String, to: String) extends Node {

    override def apply(a: DocState, enableModal: Boolean): DocState = {
      var m= a.mode0
      var zoom = a.zoom
      if (tag == data.Node.ContentType.name &&
        to == data.Node.ContentType.serialize(data.Node.ContentType.Heading(1)) &&
        a.inViewport(at) &&
        a.viewAsFolded(at, true)) {
        a.mode0 match {
          case mode.Node.Visual(a, b) =>
            val l = if (cursor.Node.contains(at, a)) at else a
            val r = if (cursor.Node.contains(at, b)) at else b
            m = mode.Node.Visual(l, r)
          case mode.Node.Content(c, d) =>
            if (cursor.Node.contains(at, c) && at != c) {
              zoom = at
            }
        }
      }
      DocState(apply(a.node), zoom, m, a.badMode, a.userFoldedNodes)
    }
    override def ty: Type = Type.Structural

    override def apply(d: data.Node): data.Node = d.map(at, nn => if (to.isEmpty) nn.clear(tag) else nn.attribute(tag, to))

    override def reverse(d: data.Node): Node = AttributeChange(at, tag, d(at).attribute(tag))

    override def merge(before: Any, whiteSpace: Boolean): Option[Node] = None

    override def isEmpty: Boolean = false

  }


  case class Content(at: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content(a.content)))
    }


    override def apply(a: DocState, enableModal: Boolean): DocState = {
      val (m0, bm) = a.mode0 match {
        case c: mode.Node.Content if c.node == at =>
          val (m, f) = content.transform(a.node(at).content, c.a, enableModal)
          (mode.Node.Content(at, m), f || a.badMode)
        case _ => (a.mode0, a.badMode)
      }
      DocState(apply(a.node), a.zoom, m0, bm, a.userFoldedNodes)
    }

    override def reverse(d: data.Node): Node = copy(content = content.reverse(d(at).content))


    override def merge(before: Any, whiteSpace: Boolean): Option[Node] = before match {
      case Content(at0, c0) if at0 == at => content.merge(c0, whiteSpace).map(a => Content(at, a))
      case _ => None
    }

    override def isEmpty: Boolean = content.isEmpty

    override def toString: String = s"Content(${at.mkString("-")}, $content)"
  }

  case class Replace(at: cursor.Node, content: data.Content) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content))
    }

    override def toString: String = s"Replace(${at.mkString("-")})"


    override def apply(a: DocState, enableModal: Boolean): DocState = {
      val (m0, bm) = a.mode0 match {
        case c: mode.Node.Content if c.node == at =>
          (mode.Node.Content(at, content.defaultMode(enableModal)), true)
        case _ => (a.mode0, a.badMode)
      }
      DocState(apply(a.node), a.zoom, m0, bm, a.userFoldedNodes)
    }

    override def reverse(d: data.Node): Node = Replace(at, d(at).content)

    override def merge(before: Any, whiteSpace: Boolean): Option[Node] = before match {
      case Replace(a, _) if a == at => Some(this)
      case Content(a, _) if a == at => Some(this)
      case _ => None
    }

    override def isEmpty: Boolean = false
  }
  case class Insert(at: cursor.Node, childs: Seq[data.Node]) extends Node {
    override def ty: Type = Type.Add
    override def apply(d: data.Node): data.Node = d.insert(at, childs)


    override def toString: String = s"Insert(${at.mkString("-")}, ${childs.size})"


    override def apply(a: DocState, enableModal: Boolean): DocState = {
      DocState(apply(a.node),
        cursor.Node.transformAfterInserted(at, childs.size, a.zoom),
        transform(a.mode0),
        a.badMode, a.userFoldedNodes)
    }

     def transform(a: mode.Node): mode.Node = {
      a match {
        case c: mode.Node.Content =>
          c.modify(_.node).using(a => cursor.Node.transformAfterInserted(at, childs.size, a))
        case mode.Node.Visual(fix, move) =>
          mode.Node.Visual(
            cursor.Node.transformAfterInserted(at, childs.size, fix),
            cursor.Node.transformAfterInserted(at, childs.size, move))
      }
    }
    override def reverse(d: data.Node): Node = Delete(range.Node(at, len = childs.size))

    override def merge(before: Any, whiteSpace: Boolean): Option[Node] = before match {
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

    override def apply(a: DocState, enableModal: Boolean): DocState = {
      val applied = apply(a.node)
      val (m0, bmm) = a.mode0  match {
        case c@mode.Node.Content(node, _) => r.transformAfterDeleted(node) match {
          case Some(k) => (c.copy(node = k), false)
          case None => (mode.Node.Content(r.parent, applied(r.parent).content.defaultMode(enableModal)), true)
        }
        case mode.Node.Visual(fix, move) =>
          (r.transformAfterDeleted(fix), r.transformAfterDeleted(move)) match {
            case (Some(ff), Some(mm)) => (mode.Node.Visual(ff, mm), false)
            case (Some(ff), None) => (mode.Node.Content(ff, applied(ff).content.defaultMode(enableModal)), true)
            case (None, Some(ff))  =>  (mode.Node.Content(ff, applied(ff).content.defaultMode(enableModal)), true)
            case (None, None) => (mode.Node.Content(r.parent, applied(r.parent).content.defaultMode(enableModal)), true)
          }
      }
      DocState(applied,
        r.transformAfterDeleted(a.zoom).getOrElse(r.parent),
        m0,
        a.badMode || bmm, a.userFoldedNodes)
    }

    override def reverse(d: data.Node): Node = Insert(r.start, d(r.parent).apply(r.childs))

    override def merge(before: Any, whiteSpace: Boolean): Option[Node] = before match {
      case Node.Content(at, _) if r.contains(at) => Some(this)
      case Node.Replace(at, _) if r.contains(at) => Some(this)
      case Node.Insert(at, childs) if r.contains(model.cursor.Node.parent(at)) => Some(this)
      case Node.Delete(rr) =>
        if (r.contains(rr.parent)) {
          Some(this)
          // this is disabled because this will causes undoer to panic
//        } else if (r.parent == rr.parent && r.childs.containsInsertion(rr.childs.start)) {
//          Some(Delete(range.Node(r.parent, IntRange(r.childs.start, r.childs.until + rr.childs.size))))
        } else {
          None
        }
      case m@Node.Move(rr, to) if r.contains(model.cursor.Node.parent(rr.transformNodeAfterMoved(to, to))) => merge(Delete(rr), whiteSpace)
      case _ => None
    }

    override def isEmpty: Boolean = r.isEmpty

  }
  case class Move(r: range.Node, to: cursor.Node) extends Node {
    assert(!r.contains(to))
    override def ty: Type = Type.Structural
    override def apply(d: data.Node): data.Node = d.move(r, to)


    override def apply(a: DocState, enableModal: Boolean): DocState = {
      var zz = r.transformNodeAfterMoved(to, a.zoom)
      val applied = apply(a.node)
      val (m0, bmm) = a.mode0  match {
        case mode.Node.Visual(fix, move) =>
          val tt = r.transformNodeAfterMoved(to, fix)
          val mm = r.transformNodeAfterMoved(to, move)
          if (cursor.Node.contains(zz, tt) && cursor.Node.contains(zz, mm)) {
            (mode.Node.Visual(tt, mm), false)
          } else {
            val alt = if (cursor.Node.contains(zz, tt)) tt
            else if (cursor.Node.contains(zz, mm)) mm
            else r.transformNodeAfterMoved(to, r.parent)
            (mode.Node.Content(alt, applied(alt).content.defaultMode(enableModal)), true)
          }
        case mode.Node.Content(node, b) =>
          val tt = r.transformNodeAfterMoved(to, node)
          if (cursor.Node.contains(zz, tt)) {
            (mode.Node.Content(tt, b), false)
          } else {
            zz = r.transformNodeAfterMoved(to, node)
            (mode.Node.Content(tt, b), false)
          }
      }
      DocState(applied,
        zz,
        m0,
        a.badMode || bmm, a.userFoldedNodes)
    }

    def reverse = operation.Node.Move(
        range.Node(cursor.Node.moveBy(r.transformNodeAfterMoved(to, to), -r.size), r.size),
        r.transformNodeAfterMoved(to, r.until)
      )

    override def reverse(d: data.Node): Node = reverse

    override def merge(before: Any, whiteSpace: Boolean): Option[Node] = None

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
        case AttributeChange(at, t, t2) =>
          writeInt(5)
          writeIntArray(at.toArray)
          writeString(t)
          writeString(t2)
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
        case 5 =>
          AttributeChange(readIntArray, readString, readString)
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
