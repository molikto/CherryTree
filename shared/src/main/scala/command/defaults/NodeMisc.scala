package command.defaults

import command.{CommandCategory, CommandInterface}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model._

class NodeMisc extends CommandCategory("node: other") {

  new TextualCommand {
    override val description: String = "remove content and change to code node"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asRichNormalOrVisual._1
      DocTransaction(
        Seq(operation.Node.Replace(cur, data.Content.Code.empty)),
        Some(a.copyContentMode(mode.Content.CodeNormal)))
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 1"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 2"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 3"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 4"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }


  new TextualCommand {
    override val description: String = "placeholder 5"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }


  new TextualCommand {
    override val description: String = "placeholder 6"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 7"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 8"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 9"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 10"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }

  new TextualCommand {
    override val description: String = "placeholder 11"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.empty
    }
  }
}
