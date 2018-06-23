package model.operation

import model._
import Type.Type
import boopickle._

import scala.util.Random


abstract sealed class Node extends Operation[data.Node] {
}

object Node extends OperationObject[data.Node, Node] {
  case class Content(c: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
    override def apply(d: data.Node): data.Node = {
      d.map(c, a => a.copy(content = content(a.content)))
    }
  }
  case class Replace(c: cursor.Node, content: data.Content) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = {
      d.map(c, a => a.copy(content = content))
    }
  }
  case class Insert(c: cursor.Node, childs: Seq[data.Node]) extends Node {
    override def ty: Type = Type.Add
    override def apply(d: data.Node): data.Node = d.insert(c, childs)
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

  override def random(d: data.Node, random: Random): Node = ???
}
