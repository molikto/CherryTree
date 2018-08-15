package model.data

import model.cursor
import model.data.Text.{Atomic, Delimited, DelimitedT}
import model.range.IntRange
import settings.SpecialKeySettings

sealed trait Atom {
  def text: Text
  def totalIndex: Int
  def subIndex: Int
  def textTotalIndex: Int = totalIndex - subIndex
  def textRange = IntRange(textTotalIndex, textTotalIndex + text.size)
  def size: Int
  def skipSize: Int = 0
  def matches(a: Unicode, delimitationCodePoints:  SpecialKeySettings): Boolean = false

  def whitespace: Boolean = false
  def letterLike: Boolean = false
  def charNonLetterLike: Boolean = false

  def nodeCursor: cursor.Node = Seq.empty


  def range = IntRange(totalIndex, totalIndex + size)

  def isAtomicWithAttribute(c: SpecialChar): Boolean = this match {
    case a: Atom.Marked  =>a.text.asDelimited.delimitation.attributes.contains(c)
    case _ => false
  }

  def isStartWithAttribute(c: SpecialChar): Boolean = this match {
    case a: Atom.Special => a.a == a.text.delimitation.start && a.text.delimitation.attributes.contains(c)
    case _ => false
  }

  def delimitationStart: Boolean = this match {
    case a: Atom.Special => a.a == a.text.delimitation.start
    case _ => false
  }

  def delimitationEnd: Boolean = this match {
    case a: Atom.Special => a.a == a.text.delimitation.end
    case _ => false
  }

  def special: Boolean = this.isInstanceOf[Atom.Special]
  def special(a: SpecialChar): Boolean = this.isInstanceOf[Atom.Special] && this.asInstanceOf[Atom.Special].a == a

  private[data] def serialize(buffer: UnicodeWriter)
}

object Atom {
  sealed trait SpecialOrMarked extends Atom {

  }
  case class Marked(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val text: Text.Atomic) extends SpecialOrMarked {
    override def subIndex: Int = 0
    override def size: Int = text.size

    override def toString: String = text.toString

    override def letterLike: Boolean = true

    override private[data] def serialize(buffer: UnicodeWriter): Unit = text.serialize(buffer)
  }

  sealed trait Special extends SpecialOrMarked {
    def a: SpecialChar
    override def size: Int = 1 + skipSize
    override def text: Delimited
    override def skipSize: Int = if (a == text.asDelimited.delimitation.end) text.asDelimited.skipSize else 0
    override def toString: String = a.toString
    override def matches(u: Unicode, delimitationCodePoints: SpecialKeySettings): Boolean = delimitationCodePoints.get(a).exists(_.startsWith(u))
    override def subIndex: Int = if (a == text.asDelimited.delimitation.start) 0 else text.size - size
    def delimitation: SpecialChar.Delimitation = text.delimitation
    def another: Special
    override def whitespace: Boolean = true

    override private[data] def serialize(buffer: UnicodeWriter): Unit = {
      if (delimitationStart) {
        buffer.put(a)
      } else {
        text.serializeAttributes(buffer)
        buffer.put(a)
      }
    }
  }

  sealed trait SpecialT[T] extends Special {
    override def text: DelimitedT[T]

  }
  case class FormattedSpecial(override val nodeCursor: cursor.Node,
    override val totalIndex: Int,
    override val a: SpecialChar,
    override val text: Text.Formatted) extends SpecialT[Seq[Text]] {
    def another: Special =
      if (delimitationStart) FormattedSpecial(nodeCursor, textTotalIndex + text.contentSize + 1, text.delimitation.end, text)
      else FormattedSpecial(nodeCursor, textTotalIndex, text.delimitation.start, text)

  }
  case class CodedSpecial(override val nodeCursor: cursor.Node,
    override val totalIndex: Int,
    override val a: SpecialChar,
    override val text: Text.Coded) extends SpecialT[Unicode] {
    def another: Special =
      if (delimitationStart) CodedSpecial(nodeCursor, textTotalIndex + text.contentSize + 1, text.delimitation.end, text)
      else CodedSpecial(nodeCursor, textTotalIndex, text.delimitation.start, text)
  }
  sealed trait Grapheme extends Atom {
    def a: Unicode
    override def matches(u: Unicode, delimitationCodePoints: SpecialKeySettings): Boolean = a == u
    override def size: Int = a.size
    override def toString: String = a.toString
    def unicodeIndex: Int
    override def letterLike: Boolean = {
      val h = a.head
      Character.isAlphabetic(h) || Character.isDigit(h) || Character.isIdeographic(h) || h == '_'.toInt
    }
    override def whitespace: Boolean = Character.isWhitespace(a.head)

    override def charNonLetterLike: Boolean = !letterLike && !whitespace

    override private[data] def serialize(buffer: UnicodeWriter): Unit = buffer.put(a)
  }
  case class PlainGrapheme(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val unicodeIndex: Int, override val a: Unicode, override val text: Text.Plain) extends Grapheme {

    override def matches(u: Unicode, delimitationCodePoints: Map[_root_.model.data.SpecialChar, Unicode]): Boolean =
      super.matches(u, delimitationCodePoints) ||
      a.str == "–" && u.str == "-" ||
        a.str == "—" && u.str == "-" ||
        a.str == "“" && u.str == "\"" ||
        a.str == "”" && u.str == "\"" ||
        a.str == "‘" && u.str == "'" ||
        a.str == "’" && u.str == "'" ||
        a.str == "…" && u.str == "."

    override def subIndex: Int = unicodeIndex
  }
  case class CodedGrapheme(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val unicodeIndex: Int, override val a: Unicode, override val text: Text.Coded) extends Grapheme {
    override def subIndex: Int = unicodeIndex + 1
  }
}
