package command.defaults

import command.{CommandCategory, CommandInterface}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model._
import model.data.Content

class NodeMisc extends CommandCategory("node: misc") {

  new TextualCommand {
    override val description: String = "change to empty code node"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asRichNormalOrVisual._1
      DocTransaction(
        Seq(operation.Node.Replace(cur, data.Content.Code.empty)),
        Some(a.copyContentMode(mode.Content.CodeNormal)))
    }
  }

  new TextualCommand {
    override val description: String = "insert rendered Markdown bellow (commonmark)"

    override protected def available(a: DocState): Boolean = a.isCodeNormal

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (cur, _) = a.asNormal
      val str = a.node(cur).content.asInstanceOf[Content.Code].unicode.str
      val node = model.parseFromCommonMarkMarkdown(str)
      DocTransaction(Seq(operation.Node.Insert(cursor.Node.moveBy(cur, 1), Seq(node))), None)
    }
  }
}
