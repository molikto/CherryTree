package command.defaults

import client.Client
import command.CommandCollector
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Rich
import model.range.IntRange
import model.{cursor, operation}

trait EnterRichInsert extends CommandCollector {


  // DIFFERENCE: currently not repeatable
  val openBellow: Command = new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: DocState): Boolean = a.isNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val pos = a.asNormal._1
      val mover = a.mover()
      val insertionPoint = if (pos == cursor.Node.root) {
        Seq(0)
      } else {
        mover.firstChild(pos).getOrElse(mover.nextOver(pos))
      }
      DocTransaction(
        Seq(operation.Node.Insert(insertionPoint, Seq(model.data.Node.empty))),
        Some(model.mode.Node.Content(insertionPoint, model.mode.Content.RichInsert(0))))
    }
  }

  val openAbove: Command = new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("O")
    override def available(a: DocState): Boolean = a.isNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val pos = a.asNormal._1
      if (pos == cursor.Node.root) {
        // LATER wrap?
        DocTransaction.empty
      } else {
        DocTransaction(
          Seq(operation.Node.Insert(pos, Seq(model.data.Node.empty))),
          Some(model.mode.Node.Content(pos, model.mode.Content.RichInsert(0))))
      }
    }
  }

  // DIFFERENCE insert mode doesn't take n currently (Sublime doesn't do this currently, wired)
  //:startinsert  :star[tinsert][!]  start Insert mode, append when [!] used
  //:startreplace :startr[eplace][!]  start Replace mode, at EOL when [!] used
  //
  //in Visual block mode:
  //v_b_I    I    insert the same text in front of all the selected lines
  //v_b_A    A    append the same text after all the selected lines

  abstract class EnterInsertCommand extends Command {
    def move(content: Rich,a: IntRange): Int

    override def available(a: DocState): Boolean = a.isRichNormal

    override def action(a: DocState, count: Int): DocTransaction =  {
      val (cursor, content, normal) = a.asRichNormal
      DocTransaction.mode(a.copyContentMode(model.mode.Content.RichInsert(move(
        content, normal.range))))
    }
  }
  val appendAtCursor: EnterInsertCommand = new EnterInsertCommand {
    override val defaultKeys: Seq[KeySeq] = Seq("a")
    override def move(content: Rich, a: IntRange): Int = a.until
  }
  val appendAtContentEnd: EnterInsertCommand  = new EnterInsertCommand {
    override val defaultKeys: Seq[KeySeq] = Seq("A")
    override def move(content: Rich,a: IntRange): Int = content.size
  }
  val insertAtCursor: EnterInsertCommand  = new EnterInsertCommand {
    override val defaultKeys: Seq[KeySeq] = Seq("i")
    override def move(content: Rich,a: IntRange): Int = a.start
  }
  val insertAtContentBeginning : EnterInsertCommand = new EnterInsertCommand {
    override val defaultKeys: Seq[KeySeq] = Seq("I", "gI") // command merged
    override def move(content: Rich,a: IntRange): Int = 0
  }
}
