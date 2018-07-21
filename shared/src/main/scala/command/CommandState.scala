package command

import command.Key.Grapheme

trait CommandState {

  def lastFindCommand: Option[(FindCommand, Grapheme)]
}
