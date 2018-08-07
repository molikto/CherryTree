package command.defaults

import command.{CommandCategory, CommandInterface}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model._

class NodeMisc extends CommandCategory("node: misc") {

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
}
