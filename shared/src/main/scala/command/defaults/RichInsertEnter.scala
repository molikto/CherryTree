package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Rich
import model.range.IntRange
import model.{cursor, operation}

class RichInsertEnter extends CommandCategory("ways to start insert text") {


  // DIFFERENCE: currently not repeatable
  new Command {
    override val description: String = "open a line bellow"
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: DocState): Boolean = a.isNormal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val pos = a.asNormal._1
      val insertionPoint = insertPoint(a, pos)
      DocTransaction(
        Seq(operation.Node.Insert(insertionPoint, Seq(model.data.Node.empty))),
        Some(model.mode.Node.Content(insertionPoint, model.mode.Content.RichInsert(0))))
    }

  }

  new Command {
    override val description: String = "open a line above"
    override val defaultKeys: Seq[KeySeq] = Seq("O")
    override def available(a: DocState): Boolean = a.isNormal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
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

  abstract class EnterInsertCommand extends Command {
    def move(content: Rich,a: IntRange): Int

    override def available(a: DocState): Boolean = a.isRichNormal

    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction =  {
      val (cursor, content, normal) = a.asRichNormal
      DocTransaction.mode(a.copyContentMode(model.mode.Content.RichInsert(move(
        content, normal.range))))
    }
  }
  new EnterInsertCommand {
    override val description: String = "append text"
    override val defaultKeys: Seq[KeySeq] = Seq("a")
    override def move(content: Rich, a: IntRange): Int = a.until
  }
  new EnterInsertCommand {
    override val description: String = "append at text end"
    override val defaultKeys: Seq[KeySeq] = Seq("A")
    override def move(content: Rich,a: IntRange): Int = content.size
  }
  new EnterInsertCommand {
    override val description: String = "insert text before cursor"
    override val defaultKeys: Seq[KeySeq] = Seq("i")
    override def move(content: Rich,a: IntRange): Int = a.start
  }
  new EnterInsertCommand {
    override val description: String = "insert text at content beginning"
    override val defaultKeys: Seq[KeySeq] = Seq("I", "gI") // command merged
    override def move(content: Rich,a: IntRange): Int = 0
  }


  // not implemented, we don't do n
  //:startinsert  :star[tinsert][!]  start Insert mode, append when [!] used
  //:startreplace :startr[eplace][!]  start Replace mode, at EOL when [!] used
  //
  //in Visual block mode:
  //v_b_I    I    insert the same text in front of all the selected lines
  //v_b_A    A    append the same text after all the selected lines
}
