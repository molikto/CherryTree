package model.operation

import model._
import Type.Type
import model.range.IntRange


sealed trait Unicode extends Operation[data.Unicode]
object Unicode {
  case class Insert(at: Int, unicode: data.Unicode) extends Unicode {
    override def ty: Type = Type.Add
    override def apply(d: data.Unicode): data.Unicode = d.insert(at, unicode)
  }
  case class Delete(override val start: Int, override val endInclusive: Int) extends IntRange with Unicode {
    override def ty: Type = Type.Delete
    override def apply(d: data.Unicode): data.Unicode = d.delete(this)
  }
  case class Move(range: range.Unicode, at: Int) extends Unicode {
    override def ty: Type = Type.Structural
    override def apply(d: data.Unicode): data.Unicode = d.move(range, at)
  }
}
