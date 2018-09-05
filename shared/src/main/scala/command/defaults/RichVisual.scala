package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface, CommandInterfaceAvailable}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Atom, SpecialChar}
import model.mode.Content.{RichInsert, RichSelection, RichVisual}
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
    override def available(a: DocState): Boolean = a.isRichNonSub
    override def actTripleClick: Boolean = true
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.mode match {
        case Some(model.mode.Node.Content(cur, rich: model.mode.Content.Rich)) =>
          val r = a.node(cur).rich
          if (!r.isEmpty) {
            return if (enableModal) {
              DocTransaction(a.copyContentMode(model.mode.Content.RichVisual(r.rangeBeginning, r.rangeEnd)))
            } else {
              DocTransaction(a.copyContentMode(model.mode.Content.RichSelection(0, r.size)))
            }
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
          case v: model.mode.Content.RichRange => true
          case _ => false
        }
      } else {
        false
      }
    }
  }

  new ShiftLeftRightCommand {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "start/expand selection left"
    override def hardcodeKeys: Seq[KeySeq] = Seq(Shift + Left)
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, mode) = a.asRich
      if (enableModal) {
        mode match {
          case model.mode.Content.RichInsert(pos) =>
            if (pos == 0) {
              DocTransaction.empty
            } else {
              DocTransaction(a.copyContentMode({
                val move = rich.rangeBefore(pos)
                RichVisual(move, move)
              }))
            }
          case v@model.mode.Content.RichVisual(fix, move) =>
            DocTransaction(a.copyContentMode({
              RichVisual(fix, rich.rangeBefore(move))
            }))
          case _ => throw new IllegalStateException("Not reachable")
        }
      } else {
        mode match {
          case model.mode.Content.RichInsert(pos) =>
            if (pos == 0) {
              DocTransaction.empty
            } else {
              DocTransaction(a.copyContentMode({
                val move = rich.before(pos).range.start
                RichSelection(pos, move)
              }))
            }
          case v@model.mode.Content.RichSelection(fix, move) =>
            val pos = move
            if (pos == 0) {
              DocTransaction.empty
            } else {
              DocTransaction(a.copyContentMode({
                val move = rich.before(pos).range.start
                if (move == fix) {
                  RichInsert(move)
                } else {
                  RichSelection(fix, move)
                }
              }))
            }
          case _ => throw new IllegalStateException("Not reachable")
        }
      }
    }
  }

  new ShiftLeftRightCommand {
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "start/expand selection right"
    override def hardcodeKeys: Seq[KeySeq] = Seq(Shift + Right)
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (_, rich, mode) = a.asRich
      if (enableModal) {
        mode match {
          case model.mode.Content.RichInsert(pos) =>
            if (pos == rich.size) {
              DocTransaction.empty
            } else {
              DocTransaction(a.copyContentMode({
                val move = rich.rangeAfter(pos)
                RichVisual(move, move)
              }))
            }
          case v@model.mode.Content.RichVisual(fix, move) =>
            DocTransaction(a.copyContentMode({
              RichVisual(fix, rich.rangeAfter(move))
            }))
          case _ => throw new IllegalStateException("Not reachable")
        }
      } else {
        mode match {
          case model.mode.Content.RichInsert(pos) =>
            if (pos == rich.size) {
              DocTransaction.empty
            } else {
              DocTransaction(a.copyContentMode({
                val move = rich.after(pos).range.until
                RichSelection(pos, move)
              }))
            }
          case v@model.mode.Content.RichSelection(fix, move) =>
            val pos = move
            if (pos == rich.size) {
              DocTransaction.empty
            } else {
              DocTransaction(a.copyContentMode({
                val move = rich.after(pos).range.until
                if (move == fix) {
                  RichInsert(move)
                } else {
                  RichSelection(fix, move)
                }
              }))
            }
          case _ => throw new IllegalStateException("Not reachable")
        }
      }
    }
  }




  new UpDownCommand(true, false, true) with OverrideCommand {
    override val description: String = "start/expand selection up"
    // DIFFERENCE we always go to first char now
    // DIFFERENCE k and - is merged
    override def hardcodeKeys: Seq[KeySeq] = Seq(Shift + Up)
  }

  new UpDownCommand(false, false, true) with OverrideCommand {
    override val description: String = "start/expand selection down"
    override def hardcodeKeys: Seq[KeySeq] = Seq(Shift + Down)
  }

  new Command {
    override def modalOnly: Boolean = true
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
          DocTransaction(a.copyContentMode(v.collapse))
        case model.mode.Content.RichInsert(i) =>
          val r = if (i == 0) rich.rangeBeginning else rich.rangeBefore(i)
          DocTransaction(a.copyContentMode(model.mode.Content.RichVisual(r, r)))
        case _ => DocTransaction.empty
      }
    }
  }

  new Command {
    override def modalOnly: Boolean = true
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override val description: String = "swap movable and fixed cursor"
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: DocState): Boolean = a.isRichVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = DocTransaction(a.copyContentMode(a.asRichVisual._3.swap))
  }

}
