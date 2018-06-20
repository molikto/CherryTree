package model.operation

import model._
import Type.Type


abstract sealed class Node extends Operation[data.Node] {
}

object Node {
  case class Content(cursor: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
    override def apply(d: data.Node): data.Node = {
      d.map(cursor, a => a.copy(content = content(a.content)))
    }
  }
  case class Replace(cursor: cursor.Node, content: data.Content) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = {
      d.map(cursor, a => a.copy(content = content))
    }
  }
  case class Insert(cursor: cursor.Node, childs: Seq[data.Node]) extends Node {
    override def ty: Type = Type.Add
    override def apply(d: data.Node): data.Node = d.insert(cursor, childs)
  }
  case class Delete(r: range.Node) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(d: data.Node): data.Node = d.delete(r)
  }
  case class Move(r: range.Node, at: cursor.Node) extends Node {
    override def ty: Type = Type.Structural
    override def apply(d: data.Node): data.Node = d.move(r, at)
  }
}
