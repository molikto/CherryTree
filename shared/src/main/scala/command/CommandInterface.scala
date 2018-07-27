package command

import command.Key.Grapheme
import doc.DocTransaction
import model.data.Unicode
import register.{RegisterInterface, Registerable}

trait CommandInterface extends RegisterInterface {

  def lastFindCommand: Option[(FindCommand, Unicode)]
  protected def commandBuffer: Seq[Part]

  def needsMotion: Boolean = commandBuffer.exists {
    case Part.IdentifiedCommand(_, c, _) => c.needsMotion
    case _ => false
  }

}
