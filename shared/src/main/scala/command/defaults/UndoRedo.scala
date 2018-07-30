package command.defaults

import command._
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Unicode

class UndoRedo extends CommandCategory("undo & redo") {


  new Command {
    override val description: String = "undo"
    override def hardcodeKeys: Seq[KeySeq] = Seq(ModKey + "z")
    override def defaultKeys: Seq[KeySeq] = Seq("u")
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def emptyAsFalseInInsertMode: Boolean = true

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      if (!a.isRichInsert || key.isEmpty || hardcodeKeys.contains(key.get)) {
        commandState.undo(a.node)
      } else {
        DocTransaction.empty
      }
    }
  }

  new Command {
    override val description: String = "redo"
    override def hardcodeKeys: Seq[KeySeq] = Seq(Seq(Key(Grapheme("z"), shift = true).modKey(true)))
    override def defaultKeys: Seq[KeySeq] = Seq(Ctrl + "r")
    override def emptyAsFalseInInsertMode: Boolean = true
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      if (!a.isRichInsert || key.isEmpty || hardcodeKeys.contains(key.get)) {
        commandState.redo(a.node)
      } else {
        DocTransaction.empty
      }
    }
  }

}
