package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface, CommandInterfaceAvailable}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Atom, SpecialChar}
import model.mode.Content.{RichInsert, RichVisual}
import model.{data, mode, operation}
import model.range.IntRange

class RichVisual extends CommandCategory("rich text: visual mode") {


//  new OverrideCommand {
//    override val description: String = "select word"
//    override def showInCommandMenu(modal: Boolean): Boolean = false
//    override def available(a: DocState): Boolean = a.isRich
//    override def documentOnly: Boolean = true
//    override def keys: Seq[KeySeq] = Seq.empty
//    override def actDoubleClick: Boolean = true
//    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
//      DocTransaction.empty
//    }
//  }

  new OverrideCommand {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "select all"
    override val hardcodeKeys: Seq[KeySeq] = Seq(ModKey + "a")
    override def available(a: DocState): Boolean = a.isRich
    override def actTripleClick: Boolean = true
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.mode match {
        case Some(model.mode.Node.Content(cur, rich: model.mode.Content.Rich)) =>
          val r = a.node(cur).rich
          if (!r.isEmpty) {
            return DocTransaction(a.copyContentMode(model.mode.Content.RichVisual(IntRange(0, 0), IntRange(r.size, r.size))))
          }
        case _ =>
      }
      DocTransaction.empty
    }
  }

  trait ShiftLeftRightCommand extends OverrideCommand {

    override def available(a: DocState): Boolean = {
      if (a.isRich) {
        val (_, _, mode) = a.asRich
        mode match {
          case model.mode.Content.RichInsert(pos) => true
          case model.mode.Content.RichVisual(a, b) => a.isEmpty
          case _ => false
        }
      } else {
        false
      }
    }
  }

  new ShiftLeftRightCommand {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "expand selection left"
    override def hardcodeKeys: Seq[KeySeq] = Seq(Shift + Left)
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, mode) = a.asRich
      mode match {
        case model.mode.Content.RichInsert(pos) =>
          if (pos == 0) {
            DocTransaction.empty
          } else {
            DocTransaction(a.copyContentMode({
              val move = rich.before(pos).range.start
              RichVisual(IntRange(pos, pos), IntRange(move, move))
            }))
          }
        case model.mode.Content.RichVisual(fix, move) =>
          val pos = move.start
          if (pos == 0) {
            DocTransaction.empty
          } else {
            DocTransaction(a.copyContentMode({
              val move = rich.before(pos).range.start
              if (move == fix.start) {
                RichInsert(move)
              } else {
                RichVisual(fix, IntRange(move, move))
              }
            }))
          }
      }
    }
  }

  new ShiftLeftRightCommand {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "expand selection right"
    override def hardcodeKeys: Seq[KeySeq] = Seq(Shift + Right)
    override def available(a: DocState): Boolean = a.isRich && !a.isRichNormal
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, mode) = a.asRich
      mode match {
        case model.mode.Content.RichInsert(pos) =>
          if (pos == rich.size) {
            DocTransaction.empty
          } else {
            DocTransaction(a.copyContentMode({
              val move = rich.after(pos).range.until
              RichVisual(IntRange(pos, pos), IntRange(move, move))
            }))
          }
        case model.mode.Content.RichVisual(fix, move) =>
          val pos = move.start
          if (pos == rich.size) {
            DocTransaction.empty
          } else {
            DocTransaction(a.copyContentMode({
              val move = rich.after(pos).range.until
              if (move == fix.start) {
                RichInsert(move)
              } else {
                RichVisual(fix, IntRange(move, move))
              }
            }))
          }
      }
    }
  }

  new Command {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "enter/exit text visual mode"
    override val defaultKeys: Seq[KeySeq] = Seq("v")
    override def available(a: DocState): Boolean = a.isNonEmptyRich
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, m) = a.asRich
      m match {
        case model.mode.Content.RichNormal(r) =>
          DocTransaction(a.copyContentMode(model.mode.Content.RichVisual(r, r)))
        case v@model.mode.Content.RichVisual(fix, move) =>
          DocTransaction(a.copyContentMode(v.collapse(enableModal)))
        case model.mode.Content.RichInsert(i) =>
          val r = if (i == 0) rich.rangeBeginning else rich.rangeBefore(i)
          DocTransaction(a.copyContentMode(model.mode.Content.RichVisual(IntRange(r.start, r.start), IntRange(r.until, r.until))))
        case _ => DocTransaction.empty
      }
    }
  }

  new Command {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "swap movable and fixed cursor"
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: DocState): Boolean = a.isRichVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = DocTransaction(a.copyContentMode(a.asRichVisual._3.swap))
  }

}
