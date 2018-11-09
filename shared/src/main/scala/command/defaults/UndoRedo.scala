package command.defaults

import command._
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Unicode
import settings.Settings

class UndoRedo(settings: Settings) extends CommandCategory(settings,"undo & redo") {


  new Command {
    override val description: String = "undo"
    override def hardcodeKeys: Seq[KeySeq] = Seq(ModKey + "z")
    override def defaultKeys: Seq[KeySeq] = Seq("u")
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      commandState.undo(a)
    }
  }

  new Command {
    override val description: String = "redo"
    override def hardcodeKeys: Seq[KeySeq] = Seq(Seq(Key(Grapheme("z"), shift = true).copyWithMod))
    override def defaultKeys: Seq[KeySeq] = Seq(Ctrl + "r")
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      commandState.redo(a)
    }
  }

}
