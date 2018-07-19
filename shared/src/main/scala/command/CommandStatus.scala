package command

import command.Key.KeySeq
import model.data.Unicode

sealed trait CommandStatus {

}

object CommandStatus {
  case object Empty extends CommandStatus
  case class InputtingCount(a: String) extends CommandStatus
  case class WaitingForConfirm(count: String, k: KeySeq) extends CommandStatus
  case class WaitingForChar(count: String, k: KeySeq) extends CommandStatus
  case class LastPerformed(count: String, k: KeySeq, char: Option[Unicode]) extends CommandStatus
  case class LastNotFound(count: String, k: KeySeq) extends CommandStatus
}
