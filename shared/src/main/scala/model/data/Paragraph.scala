package model.data

import boopickle._

import scala.util.Random

object Paragraph extends DataObject[Paragraph] {
  def serialize(content: Paragraph): Unicode = ???
  def parse(unicode: Unicode): Paragraph = ???
  def size(paragraph: Paragraph): Int = paragraph.map(_.size).sum

  override val pickler: Pickler[Paragraph] = new Pickler[Paragraph] {
    override def pickle(obj: Paragraph)(implicit state: PickleState): Unit = Unicode.pickler.pickle(serialize(obj))
    override def unpickle(implicit state: UnpickleState): Paragraph = parse(Unicode.pickler.unpickle)
  }

  override def random(random: Random): Paragraph = ???
}
