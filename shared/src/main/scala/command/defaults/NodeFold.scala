package command.defaults

import command.{CommandCategory, CommandInterface}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}

class NodeFold extends CommandCategory("node folding") {


  // TODO fold-toggle	z
  //fold-open
  //fold-close
  new Command {
    override val description: String = "toggle node folding"
    override def defaultKeys: Seq[KeySeq] = Seq("z")

    override protected def available(a: DocState): Boolean = a.isNormal

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val c = a.asNormal._1
      DocTransaction(Seq.empty, None, toggleBefore = Set(c))
    }
  }
}
