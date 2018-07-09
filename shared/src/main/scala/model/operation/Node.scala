package model.operation

import model._
import Type.Type
import model.range.IntRange

import scala.util.Random


abstract sealed class Node extends Operation[data.Node] {
  def transform(a: data.Mode): data.Mode = a
}

object Node extends OperationObject[data.Node, Node] {
  def transform(ns: Seq[Node], a: data.Mode): data.Mode = {
    a
  }

  case class Content(at: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content(a.content)))
    }
  }
  case class Replace(at: cursor.Node, content: data.Content) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = {
      d.map(at, a => a.copy(content = content))
    }
  }
  case class Insert(at: cursor.Node, childs: Seq[data.Node]) extends Node {
    override def ty: Type = Type.Add
    override def apply(d: data.Node): data.Node = d.insert(at, childs)
  }
  case class Delete(r: range.Node) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = d.delete(r)
  }
  case class Move(r: range.Node, at: cursor.Node) extends Node {
    override def ty: Type = Type.Structural
    override def apply(d: data.Node): data.Node = d.move(r, at)
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
      (cursor.Node.Root, d)
    } else {
      val c = r.nextInt(d.childs.size)
      val (aa, bb) = randomCursorAndNode(d.childs(c), r)
      (c +: aa, bb)
    }
  }

  override def random(d: data.Node, r: Random): Node = {
    // LATER generate MOVE
    def doInsert(): Node = {
      val (c, n) = randomCursorAndNode(d, r)
      val p = r.nextInt(n.childs.size + 1)
      Insert(c :+ p, (0 until r.nextInt(3)).map(_ => data.Node.random(r)))
    }
    r.nextInt(4) match {
      case 0 =>
        val (c, n) = randomCursorAndNode(d, r)
        Content(c, operation.Content.random(n.content, r))
      case 1 =>
        val (c, _) = randomCursorAndNode(d, r)
        Replace(c, data.Content.random(r))
      case 2 =>
        val (c, _) = randomCursorAndNode(d, r)
        if (c != cursor.Node.Root) {
          var ran = range.Node(c)
          if (r.nextBoolean() && ran.childs.start > 1) {
            ran = range.Node(ran.parent, IntRange(ran.childs.start - 2, ran.childs.endInclusive))
          } else if (r.nextBoolean() && ran.childs.start > 0) {
            ran = range.Node(ran.parent, IntRange(ran.childs.start - 1, ran.childs.endInclusive))
          }
          Delete(ran)
        } else {
          doInsert()
        }
      case 3 =>
        doInsert()
    }
  }
}
