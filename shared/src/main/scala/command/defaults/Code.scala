package command.defaults

import command.{CommandCategory, CommandInterface, Motion}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Unicode
import model.mode

class Code extends CommandCategory("code node") {

  new Command {
    override val description: String = "edit source code"
    override def defaultKeys: Seq[KeySeq] = Seq(Enter)
    override protected def available(a: DocState): Boolean = a.isCodeNormal
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.mode(a.copyContentMode(mode.Content.CodeInside))
    }
  }

}
