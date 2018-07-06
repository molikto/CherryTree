package model.data

import java.io.IOException

import model._
import model.range.IntRange

import scala.util.Random


class UnicodeParseException extends IOException {

}

private[model] object SpecialChar extends Enumeration {
  type Type = Value

  // LATER mmm... really? https://en.wikipedia.org/wiki/Private_Use_Areas
  val Char = '\uE700'
  val EmphasisStart = Value
  val EmphasisEnd = Value
  val StrongStart = Value
  val StrongEnd = Value
  val StrikeThroughStart = Value
  val StrikeThroughEnd = Value
  val LinkStart = Value
  val LinkContentEnd = Value
  val LinkUrlEnd = Value
  val LinkTitleEnd = Value
  val ImageStart = Value
  val ImageContentEnd = Value
  val ImageUrlEnd = Value
  val ImageTitleEnd = Value
  val CodeStart = Value
  val CodeEnd = Value
  val LaTeXStart = Value
  val LaTeXEnd = Value
  val PlainStart = Value
  val PlainEnd = Value
  val Nil = Value
}



private[model] class UnicodeWriter {

  private val sb = new StringBuilder()

  def put(a: SpecialChar.Type): Unit = {
    sb.append(SpecialChar.Char)
    sb.append(a.id.toChar)
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

  def eatOrNil(): SpecialChar.Type = {
    if (str.charAt(start) == SpecialChar.Char) {
      val c = str.charAt(start + 1)
      if (c < SpecialChar.Nil.id) {
        start = start + 2
        return SpecialChar(c)
      } else {
        throw new UnicodeParseException()
      }
    }
    SpecialChar.Nil
  }

  def eatOrFalse(a: SpecialChar.Type): Boolean = {
    if (str.charAt(start) == SpecialChar.Char) {
      if (str.charAt(start + 1) == a.id.toChar) {
        start = start + 2
        return true
      } else {
        return false
      }
    }
    false
  }

  def eatUntilSpecialChar(): Unicode = {
    var index = start
    while (str.codePointAt(index) != SpecialChar.Char.toInt) {
      index = str.offsetByCodePoints(index, 1)
    }
    val ret = str.substring(start, index)
    start = index
    Unicode(ret)
  }

  def eatUntilAndDrop(b: SpecialChar.Type): Unicode = {
    val a = eatUntilSpecialChar()
    if (!eatOrFalse(b)) throw new UnicodeParseException()
    a
  }

  def eatUntilAndDropNonEmpty(b: SpecialChar.Type): Option[Unicode] = {
    val a = eatUntilAndDrop(b)
    if (a.isEmpty) {
      None
    } else {
      Some(a)
    }
  }
}
object Unicode extends DataObject[Unicode] {
  override val pickler: boopickle.Pickler[Unicode] = new boopickle.Pickler[Unicode] {
    override def pickle(obj: Unicode)(implicit state: PickleState): Unit = state.enc.writeString(obj.toString)
    override def unpickle(implicit state: UnpickleState): Unicode = Unicode(state.dec.readString)
  }

  override def random(random: Random): Unicode = Unicode(random.nextLong().toString)
  val empty = Unicode("")
}

case class Unicode(private var str: String) {

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
    delete(r).insert(r.transformAfterDeleted(at).get, s)
  }
}
