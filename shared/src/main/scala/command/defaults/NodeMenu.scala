package command.defaults

import command.{CommandCategory, CommandInterface, Motion}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model.data.Unicode
import model.{data, mode, operation}

class NodeMenu extends CommandCategory("node menu") {

  new Command {
    override val description: String = "(DEBUG TEMP) convert node to code"
    override protected def available(a: DocState): Boolean = a.isNormal
    override def defaultKeys: Seq[KeySeq] = Seq("n")

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val cur = a.asNormal._1
      println(a.node(cur).uuid)
      DocTransaction.empty
    }
  }
}
