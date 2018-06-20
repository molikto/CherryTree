package shared.operation

import shared._
import shared.operation.Type.Type


sealed trait Unicode extends Operation[model.Unicode]
object Unicode {
  case class Insert(at: cursor.Unicode, unicode: model.Unicode) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(data: model.Unicode): model.Unicode = data.insert(at, unicode)
  }
  case class Delete(range: range.Unicode) extends Unicode {
    override def ty: Type = Type.Delete
    override def apply(data: model.Unicode): model.Unicode = data.delete(range)
  }
  case class Move(range: range.Unicode, at: cursor.Unicode) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(data: model.Unicode): model.Unicode = data.move(range, at)
  }
}
