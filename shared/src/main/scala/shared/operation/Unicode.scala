package shared.operation

import shared._
import shared.operation.Type.Type
import shared.range.IntRange


sealed trait Unicode extends Operation[model.Unicode]
object Unicode {
  case class Insert(at: Int, unicode: model.Unicode) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(data: model.Unicode): model.Unicode = data.insert(at, unicode)
  }
  case class Delete(override val start: Int, override val endInclusive: Int) extends IntRange with Unicode {
    override def ty: Type = Type.Delete
    override def apply(data: model.Unicode): model.Unicode = data.delete(this)
  }
  case class Move(range: range.Unicode, at: Int) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(data: model.Unicode): model.Unicode = data.move(range, at)
  }
}
