package shared.operation

import shared._
import shared.operation.Type.Type


abstract sealed class Node extends Operation[model.Node] {
}

object Node {
  case class Content(cursor: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
  }
  case class Replace(cursor: cursor.Node, content: model.Content) extends Node {
    override def ty: Type = Type.AddDelete
  }
  case class Insert(cursor: cursor.Node, childs: Seq[Node]) extends Node {
    override def ty: Type = Type.Add
  }
  case class Delete(range: range.Node) extends Node {
    override def ty: Type = Type.AddDelete
  }
  case class Move(range: range.Node, at: cursor.Node) extends Node {
    override def ty: Type = Type.Structural
  }
}
