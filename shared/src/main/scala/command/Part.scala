package command

import command.Key.KeySeq
import model.data.Unicode

sealed trait Part {
}

object Part {
  case class Count(a: Int) extends Part
  case class UnidentifiedCommand(key: KeySeq, commands: Seq[Command]) extends Part
  case class IdentifiedCommand(key: KeySeq, command: Command, others: Seq[Command]) extends Part
  case class UnknownCommand(key: KeySeq) extends Part
  case class Char(a: Unicode) extends Part
  case object CompleteMark extends Part
  case object UnknownPatternMark extends Part
}
