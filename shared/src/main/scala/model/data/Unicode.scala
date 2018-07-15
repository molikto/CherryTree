package model.data

import java.io.IOException
import java.util.Locale
import java.util.stream.IntStream

import model._
import model.range.IntRange
import util.GraphemeSplitter

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class UnicodeParseException(msg: String) extends IllegalStateException(msg) {

}


private[model] class UnicodeWriter {

  private val sb = new StringBuilder()

  def put(a: SpecialChar): Unit = {
    sb.append(Unicode(a))
  }

  def put(url: Unicode): Unit = {
    sb.append(url.toString)
  }

  def toUnicode: Unicode = {
    Unicode(sb.toString())
  }
}

private[model] class UnicodeReader(a: Unicode) {
  private val str = a.toString
  private var start = 0

  def isEmpty: Boolean = start >= str.length

  private def isSpecialCodePoint(c: Int) = c >= SpecialCharStart && c < SpecialCharStart + maxId

  def eatOrNotSpecial(): Option[SpecialChar] = {
    val c = str.codePointAt(start)
    if (isSpecialCodePoint(c)) {
      start = str.offsetByCodePoints(start, 1)
      Some(SpecialChar(c - SpecialCharStart))
    } else {
      None
    }
  }

  def eat(a: SpecialChar) : Unit = {
    if (!eatOrFalse(a)) {
      throw new UnicodeParseException("!!")
    }
  }

  def eatOrFalse(a: SpecialChar): Boolean = {
    if (str.codePointAt(start) == SpecialCharStart + a.id) {
      start = str.offsetByCodePoints(start, 1)
      true
    } else {
      false
    }
  }

  def eatUntilSpecialChar(): Unicode = {
    var index = start
    while (index < str.length && !isSpecialCodePoint(str.codePointAt(index))) {
      index = str.offsetByCodePoints(index, 1)
    }
    val ret = str.substring(start, index)
    start = index
    Unicode(ret)
  }

  def eatUntilAndDrop(b: SpecialChar): Unicode = {
    val a = eatUntilSpecialChar()
    if (!eatOrFalse(b)) throw new UnicodeParseException(s"Expecting $b")
    a
  }
}
object Unicode extends DataObject[Unicode] {
  override val pickler: boopickle.Pickler[Unicode] = new boopickle.Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = state.enc.writeString(obj.toString)
    override def unpickle(implicit state: UnpickleState): Unicode = Unicode(state.dec.readString)
  }

  def apply(a: SpecialChar): Unicode = {
    Unicode(new String(Character.toChars(SpecialCharStart + a.id)))
  }

  override def random(r: Random): Unicode = Unicode(r.nextLong().toString)
  val empty = Unicode("")
}

case class Unicode(private var str: String) {

  def apply(i: Int): Int = str.codePointAt(i)

  def codePoints: IntStream = str.codePoints()


  def extendedGraphemeRange(pos: Int): IntRange = {
    val strStartIndex = toStringPosition(pos)
    var start = 0
    var end = GraphemeSplitter.nextBreak(str, start) // LATER can this be simplified not iterate entire string?
    while (strStartIndex >= end) {
      start = end
      end = GraphemeSplitter.nextBreak(str, start)
    }
    val ss = str.codePointCount(0, start)
    val ee = str.codePointCount(start, end) + ss
    IntRange(ss, ee)
  }


  def toStringPosition(charPosition: Int): Int = str.offsetByCodePoints(0, charPosition)

  def join(j: Unicode): Unicode = Unicode(str + j.str)


  override def toString: String = str

  def isEmpty: Boolean = str.isEmpty
  lazy val size: Int = str.codePointCount(0, str.size)
  def slice(r: IntRange): Unicode = {
    val start = str.offsetByCodePoints(0, r.start)
    val end = str.offsetByCodePoints(start, r.size)
    Unicode(str.substring(start, end))
  }
  def insert(at: Int, u: Unicode): Unicode = {
    if (at < 0 || at > size) throw new IllegalArgumentException("Out of bound")
    val index = str.offsetByCodePoints(0, at)
    Unicode(s"${str.substring(0, index)}${u.str}${str.substring(index)}")
  }
  def delete(r: IntRange): Unicode = {
    if (r.size == 0) {
      this
    } else {
      val start = str.offsetByCodePoints(0, r.start)
      val end = str.offsetByCodePoints(start, r.size)
      Unicode(s"${str.substring(0, start)}${str.substring(end)}")
    }
  }

  def replace(r: IntRange, unicode: Unicode): Unicode = {
    val start = str.offsetByCodePoints(0, r.start)
    val end = str.offsetByCodePoints(start, r.size)
    Unicode(s"${str.substring(0, start)}${unicode.str}${str.substring(end)}")
  }

  def surround(r: IntRange, left: Unicode, right: Unicode): Unicode = {
    val start = str.offsetByCodePoints(0, r.start)
    val end = str.offsetByCodePoints(start, r.size)
    val s = str.substring(start, end)
    Unicode(s"${str.substring(0, start)}${left.str}$s${right.str}${str.substring(end)}")
  }

  def move(r: IntRange, at: Int): Unicode = {
    val s = slice(r)
    delete(r).insert(r.transformAfterDeleted(at).get, s)
  }
}
