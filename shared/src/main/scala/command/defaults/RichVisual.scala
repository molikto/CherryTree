package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Atom, SpecialChar}
import model.{data, mode, operation}
import model.range.IntRange

class RichVisual extends CommandCategory("rich text: visual mode") {


  new OverrideCommand {
    override val description: String = "select all"
    override val hardcodeKeys: Seq[KeySeq] = Seq(ModKey + "a")
    override def available(a: DocState): Boolean = true
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.mode match {
        case Some(model.mode.Node.Content(cur, rich: model.mode.Content.Rich)) =>
          val r = a.node(cur).rich
          if (!r.isEmpty) {
            return DocTransaction(a.copyContentMode(model.mode.Content.RichVisual(r.rangeBeginning, r.rangeEnd)))
          }
      }
      DocTransaction.empty
    }
  }

  new Command {
    override val description: String = "enter/exit text visual mode"
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
        case v@model.mode.Content.RichVisual(fix, move) =>
          DocTransaction(a.copyContentMode(v.collapse(enableModal)))
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
