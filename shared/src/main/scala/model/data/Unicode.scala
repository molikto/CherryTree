package model.data

import model._


object Unicode {
  def apply(str: String): Unicode = Unicode(str.codePoints().toArray)
}
case class Unicode(str: Array[Int]) {

  override def toString: String = new String(str, 0, str.length)

  def isEmpty: Boolean = str.isEmpty
  def size: Int = str.size
  def slice(r: range.Unicode): Unicode = {
    Unicode(str.slice(r.start, r.until))
  }
  def insert(at: Int, u: Unicode): Unicode = {
    if (at > str.size) throw new IllegalArgumentException()
    Unicode(str.patch(at, u.str, 0))
  }
  def delete(r: range.Unicode): Unicode = {
    Unicode(str.patch(r.start, Seq.empty, r.size))
  }
  def move(r: range.Unicode, at: Int): Unicode = {
    val s = slice(r)
    delete(r).insert(r.transformInsertionPointAfterDeleted(at), s)
  }
}
