package command

import command.Key.Grapheme
import model.data.Unicode

trait CommandState {

  def lastFindCommand: Option[(FindCommand, Unicode)]
  def commandBuffer: Seq[Part]

  def needsMotion: Boolean = commandBuffer.exists {
    case Part.IdentifiedCommand(_, c, _) => c.needsMotion
    case _ => false
  }
}
