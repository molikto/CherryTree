package command

import doc.{DocState, DocTransaction}
import model.data.{Rich, Unicode}
import model.range.IntRange

trait FindCommand extends Command with RichMotion {


  def reverse: FindCommand

  override def move(commandState: CommandInterface, content: Rich, count: Int, r: IntRange, char: Option[Unicode]): Option[(IntRange, Int)] = {
    char.flatMap(c => findGrapheme(content, r, c, count, skipCurrent = false)).map(a => (a, 1))
  }

  def findGrapheme(a: Rich, r: IntRange, char: Unicode, count: Int, skipCurrent: Boolean): Option[IntRange]

  def action(a: DocState, char: Unicode, count: Int, skipCurrent: Boolean): DocTransaction = {
    val (_, content, mm) = a.asRichNormalOrVisual
    def act(r: IntRange) = findGrapheme(content, mm.focus, char, count, skipCurrent)
    act(mm.focus) match {
      case Some(move) =>
        DocTransaction.mode(a.copyContentMode(mm.copyWithNewFocus(move)))
      case None =>
        DocTransaction.empty
    }
  }
}
