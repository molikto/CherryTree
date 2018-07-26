package model.data

import model.cursor
import model.data.Text.{Atomic, Delimited}
import model.range.IntRange

sealed trait Atom {
  def text: Text
  def totalIndex: Int
  def subIndex: Int
  def textTotalIndex: Int = totalIndex - subIndex
  def textRange = IntRange(textTotalIndex, textTotalIndex + text.size)
  def size: Int
  def skipSize: Int = 0
  def matches(a: Unicode, delimitationCodePoints:  Map[SpecialChar, Unicode]): Boolean = false

  def whitespace: Boolean = false
  def letterLike: Boolean = false
  def charNonLetterLike: Boolean = false

  def nodeCursor: cursor.Node = Seq.empty


  def range = IntRange(totalIndex, totalIndex + size)

  def delimitationStart: Boolean = this match {
    case a: Atom.Special[Any] => a.a == a.text.delimitation.start
    case _ => false
  }

  def delimitationEnd: Boolean = this match {
    case a: Atom.Special[Any] => a.a == a.text.delimitation.end
    case _ => false
  }

  def special: Boolean = this.isInstanceOf[Atom.Special[Any]]
  def special(a: SpecialChar): Boolean = this.isInstanceOf[Atom.Special[Any]] && this.asInstanceOf[Atom.Special[Any]].a == a
}

object Atom {
  sealed trait SpecialOrMarked extends Atom {

  }
  case class Marked(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val text: Text.Atomic) extends SpecialOrMarked {
    override def subIndex: Int = 0
    override def size: Int = text.size

    override def toString: String = text.toString

    override def letterLike: Boolean = true
  }
  sealed trait Special[T] extends SpecialOrMarked {
    def a: SpecialChar
    override def text: Delimited[T]
    override def size: Int = 1
    override def skipSize: Int = if (a == text.asDelimited.delimitation.end) text.asDelimited.skipSize else 0
    override def toString: String = a.toString
    override def matches(u: Unicode, delimitationCodePoints: Map[SpecialChar, Unicode]): Boolean = delimitationCodePoints.get(a).contains(u)
    override def subIndex: Int = if (a == text.asDelimited.delimitation.start) 0 else text.size - 1
    def delimitation: SpecialChar.Delimitation = text.delimitation
    def another: Atom
    override def whitespace: Boolean = true
  }
  case class FormattedSpecial(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val a: SpecialChar, override val text: Text.Formatted) extends Special[Seq[Text]] {
    def another: Atom =
      if (delimitationStart) FormattedSpecial(nodeCursor, textTotalIndex + text.size - 1, text.delimitation.end, text)
      else FormattedSpecial(nodeCursor, textTotalIndex, text.delimitation.start, text)
  }
  case class CodedSpecial(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val a: SpecialChar, override val text: Text.Coded) extends Special[Unicode] {
    def another: Atom =
      if (delimitationStart) CodedSpecial(nodeCursor, textTotalIndex + text.size - 1, text.delimitation.end, text)
      else CodedSpecial(nodeCursor, textTotalIndex, text.delimitation.start, text)
  }
  sealed trait Grapheme extends Atom {
    def a: Unicode
    override def matches(u: Unicode, delimitationCodePoints: Map[SpecialChar, Unicode]): Boolean = a == u
    override def size: Int = a.size
    override def toString: String = a.toString
    def unicodeIndex: Int
    override def letterLike: Boolean = {
      val h = a.head
      Character.isAlphabetic(h) || Character.isDigit(h) || Character.isIdeographic(h) || h == '_'.toInt
    }
    override def whitespace: Boolean = Character.isWhitespace(a.head)

    override def charNonLetterLike: Boolean = !letterLike && !whitespace
  }
  case class PlainGrapheme(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val unicodeIndex: Int, override val a: Unicode, override val text: Text.Plain) extends Grapheme {
    override def subIndex: Int = unicodeIndex
  }
  case class CodedGrapheme(override val nodeCursor: cursor.Node, override val totalIndex: Int, override val unicodeIndex: Int, override val a: Unicode, override val text: Text.Coded) extends Grapheme {
    override def subIndex: Int = unicodeIndex + 1
  }
}
