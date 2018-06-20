package shared.operation

import shared._
import shared.operation.Type.Type


abstract sealed class Node extends Operation[model.Node] {
}

object Node {
  case class Content(cursor: cursor.Node, content: operation.Content) extends Node {
    override def ty: Type = content.ty
    override def apply(data: model.Node): model.Node = {
      data.map(cursor, a => a.copy(content = content(a.content)))
    }
  }
  case class Replace(cursor: cursor.Node, content: model.Content) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(data: model.Node): model.Node = {
      data.map(cursor, a => a.copy(content = content))
    }
  }
  case class Insert(cursor: cursor.Node, childs: Seq[model.Node]) extends Node {
    override def ty: Type = Type.Add
    override def apply(data: model.Node): model.Node = data.insert(cursor, childs)
  }
  case class Delete(r: range.Node) extends Node {
    override def ty: Type = Type.AddDelete
    override def apply(data: model.Node): model.Node = data.delete(r)
  }
  case class Move(r: range.Node, at: cursor.Node) extends Node {
    override def ty: Type = Type.Structural
    override def apply(data: model.Node): model.Node = data.move(r, at)
  }
}
