package shared.model

import shared._


object Unicode {
  def apply(str: String): Unicode = Unicode(str.codePoints().toArray)
}
case class Unicode(str: Array[Int]) {

  override def toString: String = new String(str, 0, str.length)

  def isEmpty = str.isEmpty
  def size = str.size
  def slice(range: range.Unicode): Unicode = {
    Unicode(str.slice(range.start, range.endInclusive + 1))
  }
  def insert(at: cursor.Unicode, u: Unicode): Unicode = {
    if (at > str.size) throw new IllegalArgumentException()
    Unicode(str.take(at) ++ u.str ++ str.drop(at))
  }
  def delete(range: range.Unicode): Unicode = {
    Unicode(str.take(range.start) ++ str.drop(range.endInclusive + 1))
  }
  def move(range: range.Unicode, at: Int): Unicode = {
    val s= slice(range)
    if (at < range.start) {
      delete(range).insert(at, s)
    } else if (at > range.endInclusive) {
      delete(range).insert(at - s.size, s)
    } else {
      throw new AssertionError()
    }
  }
}
