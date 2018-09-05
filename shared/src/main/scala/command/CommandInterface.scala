package command

import command.Key.Grapheme
import doc.DocTransaction
import model.data.Unicode
import register.{RegisterInterface, Registerable}
import search.{SearchHandler, StartSearchInterface}
import undoer.UndoerInterface

trait CommandInterfaceAvailable {
  def lastFindCommand: Option[(FindCommand, Unicode)]
  protected def commandBuffer: Seq[Part]

  def needsMotion: Boolean = commandBuffer.exists {
    case Part.IdentifiedCommand(_, c, _) => c.needsMotion
    case _ => false
  }

}

trait CommandInterface extends CommandInterfaceAvailable with RegisterInterface with UndoerInterface with StartSearchInterface {


}
