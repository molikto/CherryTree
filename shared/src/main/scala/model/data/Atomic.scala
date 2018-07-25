package model.data

import model.data.Text.{AtomicMark, Delimited}

trait Atomic {
  def text: Text
}

object Atomic {
  case class Marked(override val text: Text.AtomicMark) extends Atomic
  case class FormattedSpecial(a: SpecialChar, override val text: Text.Formatted) extends Atomic
  case class CodedSpecial(a: SpecialChar, override val text: Text.Coded) extends Atomic
  case class PlainGrapheme(a: Unicode, override val text: Text.Plain) extends Atomic
  case class CodedGrapheme(a: Unicode, override val text: Text.Coded) extends Atomic
}
