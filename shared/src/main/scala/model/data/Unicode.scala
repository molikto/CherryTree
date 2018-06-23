package model.data

import boopickle._
import model._
import model.range.IntRange

import scala.util.Random


object Unicode extends DataObject[Unicode] {
  override val pickler: Pickler[Unicode] = new Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = state.enc.writeString(obj.toString)
    override def unpickle(implicit state: UnpickleState): Unicode = Unicode(state.dec.readString)
  }

  override def random(random: Random): Unicode = Unicode(random.nextLong().toString)
}
case class Unicode(private val str: String) {

  override def toString: String = str

  def isEmpty: Boolean = str.isEmpty
  lazy val size: Int = str.codePointCount(0, str.size)
  def slice(r: IntRange): Unicode = {
    val start = str.offsetByCodePoints(0, r.start)
    val end = str.offsetByCodePoints(start, r.size)
    Unicode(str.substring(start, end))
  }
  def insert(at: Int, u: Unicode): Unicode = {
    if (at > size) throw new IllegalArgumentException()
    val index = str.offsetByCodePoints(0, at)
    Unicode(s"${str.substring(0, index)}${u.str}${str.substring(index)}")
  }
  def delete(r: IntRange): Unicode = {
    val start = str.offsetByCodePoints(0, r.start)
    val end = str.offsetByCodePoints(start, r.size)
    Unicode(s"${str.substring(0, start)}${str.substring(end)}")
  }

  def move(r: IntRange, at: Int): Unicode = {
    val s = slice(r)
    delete(r).insert(r.transformInsertionPointAfterDeleted(at), s)
  }
}
