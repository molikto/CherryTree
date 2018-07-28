package command

import command.Key.KeySeq
import model.data.Unicode

sealed trait Part {
}

object Part {
  trait Finished extends Part
  case class Count(a: Int) extends Part
  case class UnidentifiedCommand(key: KeySeq, commands: Seq[Command]) extends Part
  case class IdentifiedCommand(key: KeySeq, command: Command, others: Seq[Command]) extends Part
  case class Char(a: Unicode) extends Part
  case class UnknownCommand(key: KeySeq) extends Finished
  case object CompleteMark extends Finished
  case object UnknownPatternMark extends Finished
}
