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

  private var size = 0

  def put(a: SpecialChar): Unit = {
    sb.append(Unicode(a))
    size += 1
  }

  def put(url: Unicode): Unit = {
    sb.append(url.str)
    size += url.size
  }

  def toUnicode: Unicode = {
    Unicode(sb.toString(), size)
  }
}

private[model] class UnicodeReader(a: Unicode) {
  private val str = a.str
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
    var codePointCount = 0
    while (index < str.length && !isSpecialCodePoint(str.codePointAt(index))) {
      index = str.offsetByCodePoints(index, 1)
      codePointCount += 1
    }
    val ret = str.substring(start, index)
    start = index
    Unicode(ret, codePointCount)
  }

  def eatUntilAndDrop(b: SpecialChar): Unicode = {
    val a = eatUntilSpecialChar()
    if (!eatOrFalse(b)) throw new UnicodeParseException(s"Expecting $b")
    a
  }
}
object Unicode extends DataObject[Unicode] {
  override val pickler: boopickle.Pickler[Unicode] = new boopickle.Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = state.enc.writeString(obj.str)
    override def unpickle(implicit state: UnpickleState): Unicode = Unicode(state.dec.readString)
  }

  def apply(a: String, size: Int): Unicode = {
    val u = Unicode(a)
    u.size0 = size
    u
  }

  def apply(a: SpecialChar): Unicode = {
    Unicode(new String(Character.toChars(SpecialCharStart + a.id)), 1)
  }

  def apply(a: Seq[SpecialChar]): Unicode = {
    Unicode(a.map(apply).mkString, a.size)
  }


  def ofCodePoints(a: Seq[Int]): Unicode = {
    Unicode(a.map(apply).mkString, a.size)
  }

  def apply(a: Int): Unicode = {
    Unicode(new String(Character.toChars(a)), 1)
  }

  override def random(r: Random): Unicode = Unicode(r.nextLong().toString)
  val empty = Unicode("", 0)
}

case class Unicode(var str: String) {

  def times(size: Int): Unicode = {
    Unicode(str * size, if (size0 == -1) -1 else size0 * size)
  }

  def size: Int = {
    if (size0 == -1) {
      size0 = str.codePointCount(0, str.length)
    }
    size0
  }

  private var lastCharIndex = -1
  private var lastCodePointIndex = -1
  private var size0 = -1
  def apply(i: Int): Int = str.codePointAt(toStringPosition(i))

  def toStringPosition(i: Int): Int = {
    if (noSurrogatePairBeforeAndAtCodePointIndex(i)) {
      i
    } else {
      println(s"complex requested codepoint $str, $i, with $lastCharIndex, $lastCodePointIndex")
      val index = if (lastCharIndex == -1 || i * 2 <= lastCodePointIndex) {
        str.offsetByCodePoints(0, i)
      } else {
        str.offsetByCodePoints(lastCharIndex, i - lastCodePointIndex)
      }
      lastCharIndex = index
      lastCodePointIndex = i
      index
    }
  }

  def codePoints: Seq[Int] = {
    val a = new ArrayBuffer[Int]()
    var i = 0
    while (i < str.length) {
      a.append(str.codePointAt(i))
      i = str.offsetByCodePoints(i, 1)
    }
    a
  }

  private def noSurrogatePairBeforeAndAtCodePointIndex(pos: Int): Boolean = {
    if (size0 != -1) {
      if (size0 == str.length) {
        return true
      }
    }
    lastCharIndex == lastCodePointIndex && pos < lastCodePointIndex
  }

  private def extendedGraphemeStrRange(pos: Int): (Int, Int) = {
    // if this part is a very simple char
    val strStartIndex = toStringPosition(pos)
    var start = 0
    var end = GraphemeSplitter.nextBreak(str, start) // LATER can this be simplified not iterate entire string?
    var count = 1
    while (strStartIndex >= end) {
      start = end
      end = GraphemeSplitter.nextBreak(str, start)
      count += 1
    }
    (start, end)
  }

  def graphemesCount: Int = {
    if (isEmpty) {
      0
    } else {
      var start = 0
      var end = GraphemeSplitter.nextBreak(str, start) // LATER can this be simplified not iterate entire string?
      var size = 1
      while (end < str.length) {
        start = end
        end = GraphemeSplitter.nextBreak(str, start)
        size += 1
      }
      size
    }
  }

  def extendedGrapheme(pos: Int): Unicode = {
    val (start, end) = extendedGraphemeStrRange(pos)
    Unicode(str.substring(start, end))
  }

  def extendedGraphemeRange(pos: Int): IntRange = {
    val (start, end) = extendedGraphemeStrRange(pos)
    val ss = str.codePointCount(0, start)
    val ee = str.codePointCount(start, end) + ss
    IntRange(ss, ee)
  }



  def join(j: Unicode): Unicode = Unicode(str + j.str, if (size0 == -1 || j.size0 == -1) -1 else size0 + j.size0)


  override def toString: String = str

  def isEmpty: Boolean = str.isEmpty
  def slice(r: IntRange): Unicode = {
    val start = toStringPosition(r.start)
    val end = toStringPosition(r.until)
    Unicode(str.substring(start, end), r.size)
  }
  def insert(at: Int, u: Unicode): Unicode = {
    if (at < 0 || at > size) throw new IllegalArgumentException("Out of bound")
    val index = toStringPosition(at)
    Unicode(s"${str.substring(0, index)}${u.str}${str.substring(index)}", if (size0 == -1 || u.size0 == -1) -1 else size0 + u.size0)
  }
  def delete(r: IntRange): Unicode = {
    if (r.size == 0) {
      this
    } else {
      val start = toStringPosition(r.start)
      val end = toStringPosition(r.until)
      Unicode(s"${str.substring(0, start)}${str.substring(end)}", if (size0 == -1) -1 else size0 - r.size)
    }
  }

  def replace(r: IntRange, unicode: Unicode): Unicode = {
    val start = toStringPosition(r.start)
    val end = toStringPosition(r.until)
    Unicode(s"${str.substring(0, start)}${unicode.str}${str.substring(end)}", if (size0 == -1 || unicode.size0 == -1) -1 else size0 -r.size + unicode.size0)
  }

  def surround(r: IntRange, left: Unicode, right: Unicode): Unicode = {
    val start = toStringPosition(r.start)
    val end = toStringPosition(r.until)
    val s = str.substring(start, end)
    Unicode(s"${str.substring(0, start)}${left.str}$s${right.str}${str.substring(end)}", if (size0 == -1 || left.size0 == -1 || right.size0 == -1) -1 else size0 + left.size0 + right.size0)
  }

  def move(r: IntRange, at: Int): Unicode = {
    val s = slice(r)
    delete(r).insert(r.transformAfterDeleted(at).get, s)
  }

  val isDigit: Boolean = str.length == 1 && Character.isDigit(str.charAt(0))

  def asDigit: Int = str.toInt
}
