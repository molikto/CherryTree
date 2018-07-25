package command

import doc.{DocState, DocTransaction}
import model.data.Unicode

trait FindCommand extends Command {


  def reverse: FindCommand

  def findGrapheme(a: DocState, char: Unicode, count: Int, skipCurrent: Boolean): DocTransaction
}
