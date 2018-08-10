package command.defaults

import command.{CommandCategory, CommandInterface, Key}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model.mode

class NodeFold extends CommandCategory("node folding") {

  new Command {
    override val description: String = "zoom into node"
    override def defaultKeys: Seq[KeySeq] = Seq(Key.Enter)

    override protected def available(a: DocState): Boolean = a.isNormal

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty.copy(zoom = Some(a.asNormal._1))
    }
  }

  new Command {
    override val description: String = "zoom into parent"
    override def defaultKeys: Seq[KeySeq] = Seq("[")

    override protected def available(a: DocState): Boolean = a.isNormal && a.zoom.nonEmpty

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val curZoom =a.node(a.zoom)
      DocTransaction(Seq.empty,
        if (a.userFoldedNodes.contains(curZoom.uuid)) Some(mode.Node.Content(a.zoom, curZoom.content.defaultNormalMode()))
        else None,
        zoom = Some(a.zoom.dropRight(1)))
    }
  }

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
