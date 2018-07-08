package model.data

import java.io.IOException

import model._
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class UnicodeParseException(msg: String) extends IOException(msg) {

}

private[model] object SpecialChar extends Enumeration {
  type Type = Value

  // LATER mmm... really? https://en.wikipedia.org/wiki/Private_Use_Areas
  val Start = 0xF0000

  def toUnicode(t: Type): Unicode = Unicode(s"$Char${t.id.toChar}")

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

  val formatted = Seq(
    (EmphasisStart, EmphasisEnd),
    (StrongStart, StrongEnd),
    (StrikeThroughStart, StrikeThroughEnd)
  )

  val linked = Seq(
    (LinkStart, LinkContentEnd, LinkUrlEnd, LinkTitleEnd),
    (ImageStart, ImageContentEnd, ImageUrlEnd, ImageTitleEnd)
  )

  val coded = Seq(
    (CodeStart, CodeEnd),
    (LaTeXStart, LaTeXEnd)
  )
}


private[model] class UnicodeWriter {

  private val sb = new StringBuilder()

  def put(a: SpecialChar.Type): Unit = {
    Character.toChars(SpecialChar.Start + a.id).foreach(sb.append)
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

  private def isSpecialCodePoint(c: Int) = c >= SpecialChar.Start && c < SpecialChar.Start + SpecialChar.maxId

  def eatOrNotSpecial(): Option[SpecialChar.Type] = {
    val c = str.codePointAt(start)
    if (isSpecialCodePoint(c)) {
      start = str.offsetByCodePoints(start, 1)
      Some(SpecialChar(c - SpecialChar.Start))
    } else {
      None
    }
  }

  def eatOrFalse(a: SpecialChar.Type): Boolean = {
    if (str.codePointAt(start) == SpecialChar.Start + a.id) {
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

  def eatUntilAndDrop(b: SpecialChar.Type): Unicode = {
    val a = eatUntilSpecialChar()
    if (!eatOrFalse(b)) throw new UnicodeParseException(s"Expecting $b")
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

  override def random(r: Random): Unicode = Unicode(r.nextLong().toString)
  val empty = Unicode("")
}

case class Unicode(private var str: String) {

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
    if (at > size) throw new IllegalArgumentException()
    val index = str.offsetByCodePoints(0, at)
    Unicode(s"${str.substring(0, index)}${u.str}${str.substring(index)}")
  }
  def delete(r: IntRange): Unicode = {
    val start = str.offsetByCodePoints(0, r.start)
    val end = str.offsetByCodePoints(start, r.size)
    Unicode(s"${str.substring(0, start)}${str.substring(end)}")
  }

  def replace(r: IntRange, unicode: Unicode): Unicode = {
    val start = str.offsetByCodePoints(0, r.start)
    val end = str.offsetByCodePoints(start, r.size)
    Unicode(s"${str.substring(0, start)}${unicode.str}${str.substring(end)}")
  }

  def move(r: IntRange, at: Int): Unicode = {
    val s = slice(r)
    delete(r).insert(r.transformAfterDeleted(at).get, s)
  }
}
