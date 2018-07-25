package command

import command.Key.Grapheme
import model.data.Unicode

trait CommandState {

  def lastFindCommand: Option[(FindCommand, Unicode)]
}
