package model.operation

import boopickle.{PickleState, Pickler, UnpickleState}
import model.{data, _}
import model.operation.Type.Type


case class Paragraph(u: Unicode) extends Operation[data.Paragraph] {
  override def ty: Type = u.ty
  override def apply(d: data.Paragraph): data.Paragraph =
    data.Paragraph.parse(u(data.Paragraph.serialize(d)))
}

object Paragraph {

  val pickler: Pickler[Paragraph] = new Pickler[Paragraph] {
    override def pickle(obj: Paragraph)(implicit state: PickleState): Unit = {
      Unicode.pickler.pickle(obj.u)
    }

    override def unpickle(implicit state: UnpickleState): Paragraph = {
      Paragraph(Unicode.pickler.unpickle)
    }
  }
}
