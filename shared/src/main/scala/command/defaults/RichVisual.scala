package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Atom, SpecialChar}
import model.{data, mode, operation}
import model.range.IntRange

class RichVisual extends CommandCategory("text visual mode") {


  new Command {
    override val description: String = "enter text visual mode"
    override val defaultKeys: Seq[KeySeq] = Seq("v")
    override def available(a: DocState): Boolean = a.isNonEmptyRichNormalOrVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, m) = a.asRichNormalOrVisual
      m match {
        case model.mode.Content.RichNormal(r) =>
          if (rich.isEmpty) {
            DocTransaction.empty
          } else {
            DocTransaction(a.copyContentMode(model.mode.Content.RichVisual(r, r)))
          }
        case model.mode.Content.RichVisual(fix, move) =>
          DocTransaction(a.copyContentMode(model.mode.Content.RichNormal(move)))
      }
    }

  }

  new Command {
    override val description: String = "swap movable and fixed cursor"
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: DocState): Boolean = a.isRichVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = DocTransaction(a.copyContentMode(a.asRichVisual._3.swap))
  }

}
