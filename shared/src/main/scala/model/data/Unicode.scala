package model.data

import model._


object Unicode {
  def apply(str: String): Unicode = Unicode(str.codePoints().toArray)
}
case class Unicode(str: Array[Int]) {

  override def toString: String = new String(str, 0, str.length)

  def isEmpty = str.isEmpty
  def size = str.size
  def slice(range: range.Unicode): Unicode = {
    Unicode(str.slice(range.start, range.until))
  }
  def insert(at: Int, u: Unicode): Unicode = {
    if (at > str.size) throw new IllegalArgumentException()
    Unicode(str.patch(at, u.str, 0))
  }
  def delete(range: range.Unicode): Unicode = {
    Unicode(str.patch(range.start, Seq.empty, range.size))
  }
  def move(range: range.Unicode, at: Int): Unicode = {
    val s = slice(range)
    delete(range).insert(range.transformInsertionPointAfterDeleted(at), s)
  }
}
