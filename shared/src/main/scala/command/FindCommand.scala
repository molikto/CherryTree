package command

import command.Key.Grapheme
import doc.{DocState, DocTransaction}

trait FindCommand extends Command {


  def reverse: FindCommand

  def findGrapheme(a: DocState, char: Grapheme, count: Int, skipCurrent: Boolean): DocTransaction
}
