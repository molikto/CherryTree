package model.data

import boopickle._

object Paragraph {
  def serialize(content: Paragraph): Unicode = ???
  def parse(unicode: Unicode): Paragraph = ???
  def size(paragraph: Paragraph): Int = paragraph.map(_.size).sum

  val pickler: Pickler[Paragraph] = new Pickler[Paragraph] {
    override def pickle(obj: Paragraph)(implicit state: PickleState): Unit = Unicode.pickler.pickle(serialize(obj))
    override def unpickle(implicit state: UnpickleState): Paragraph = parse(Unicode.pickler.unpickle)
  }
}
