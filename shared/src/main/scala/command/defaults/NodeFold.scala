package command.defaults

import command.{CommandCategory, CommandInterface, Key}
import command.Key._
import doc.{DocState, DocTransaction}
import model.{cursor, mode}

class NodeFold extends CommandCategory("node: fold & zoom") {

  new Command {
    override val description: String = "zoom into node"
    override def defaultKeys: Seq[KeySeq] = Seq(Key.Enter)


    override def priority: Int = 0

    override protected def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty.copy(zoomAfter = Some(a.asContent))
    }
  }

  new Command {
    override val description: String = "zoom into parent"
    override def defaultKeys: Seq[KeySeq] = Seq("[")


    override protected def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      if (a.zoom == cursor.Node.root) {
        DocTransaction.empty
      } else {
        val curZoom = a.node(a.zoom)
        DocTransaction(Seq.empty,
          if (a.folded(a.zoom)) Some(mode.Node.Content(a.zoom, curZoom.content.defaultMode(enableModal)))
          else None,
          zoomAfter = Some(model.cursor.Node.parent(a.zoom)))
      }
    }
  }

  new Command {
    override val description: String = "toggle node folding"
    override def defaultKeys: Seq[KeySeq] = Seq("z")

    override protected def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val c = a.asContent
      if (c != a.zoom) {
        DocTransaction(Seq.empty, None, toggleBefore = Set(c))
      } else {
        DocTransaction.empty
      }
    }
  }

  new Command {
    override val description: String = "unfold node"
    override def defaultKeys: Seq[KeySeq] = Seq(ModKey + Down)

    override protected def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val c = a.asContent
      if (a.viewAsFolded(c)) {
        DocTransaction(Seq.empty, None, toggleBefore = Set(c))
      } else {
        DocTransaction.empty
      }
    }
  }

  new Command {
    override val description: String = "fold node"
    override def defaultKeys: Seq[KeySeq] = Seq(ModKey + Up)

    override protected def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val c = a.asContent
      if (c != a.zoom && !a.viewAsFolded(c)) {
        DocTransaction(Seq.empty, None, toggleBefore = Set(c))
      } else {
        DocTransaction.empty
      }
    }
  }
}
