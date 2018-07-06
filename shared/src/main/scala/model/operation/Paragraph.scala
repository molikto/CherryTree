package model.operation

import model.{data, _}
import model.operation.Type.Type

import scala.util.Random


/**
  * it is not intended to create paragraph operations directly via
  */
case class Paragraph(u: Seq[Unicode], override val ty: Type) extends Operation[data.Paragraph] {
  override def apply(d: data.Paragraph): data.Paragraph =
    data.Paragraph.parse(Unicode.apply(u, data.Paragraph.serialize(d)))
}

object Paragraph extends OperationObject[data.Paragraph, Paragraph] {

  override val pickler: Pickler[Paragraph] = new Pickler[Paragraph] {
    override def pickle(obj: Paragraph)(implicit state: PickleState): Unit = {
      import state.enc._
      writeInt(obj.u.size)
      obj.u.foreach(a => Unicode.pickler.pickle(a))
      writeInt(obj.ty.id)
    }

    override def unpickle(implicit state: UnpickleState): Paragraph = {
      import state.dec._
      Paragraph((0 until readInt).map(_ => Unicode.pickler.unpickle), Type(readInt))
    }
  }

  override def random(d: data.Paragraph, random: Random): Paragraph = ???

  override def apply(cs: TRANSACTION, model: data.Paragraph): data.Paragraph = {
    data.Paragraph.parse(cs.foldLeft(data.Paragraph.serialize(model)) { (m, c) =>
      Unicode.apply(c.u, m)
    })
  }

  override def applyT(cs: Seq[TRANSACTION], model: data.Paragraph): data.Paragraph = {
    data.Paragraph.parse(cs.foldLeft(data.Paragraph.serialize(model)) { (m, t) =>
      t.foldLeft(m) { (m, c) => Unicode.apply(c.u, m) }
    })
  }
}
