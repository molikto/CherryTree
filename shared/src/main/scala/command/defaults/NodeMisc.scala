package command.defaults

import command.{CommandCategory, CommandInterface}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model._
import model.data.{Content, Text}

class NodeMisc extends CommandCategory("node: misc") {

  new TextualCommand {
    override val description: String = "reset content to: code"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrVisual
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asRichNormalOrVisual._1
      DocTransaction(
        Seq(operation.Node.Replace(cur, data.Content.Code.empty)),
        Some(a.copyContentMode(mode.Content.CodeNormal)))
    }
  }

  new TextualCommand {
    override val description: String = "reset content to: rich text"
    override protected def available(a: DocState): Boolean = a.isCodeNormal
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (cur, code) = a.asCodeNormal
      DocTransaction(
        Seq(operation.Node.Replace(cur, data.Content.Rich(data.Rich(Seq(Text.Plain(code.unicode)))))),
        Some(a.copyContentMode(mode.Content.CodeNormal)))
    }
  }


  class ContentStyleCommand(desc: String, to: Option[data.Node.ContentType]) extends TextualCommand {
    override val description: String = s"content style: $desc"
    override protected def available(a: DocState): Boolean = a.isNormal
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asNormal._1
      DocTransaction(
        Seq(operation.Node.AttributeChange(cur, data.Node.ContentType, to)),
        a.mode)
    }
  }

  for (i <- 1 to 6) {
    new ContentStyleCommand(s"heading $i (h$i)", Some(data.Node.ContentType.Heading(i)))
  }

  new ContentStyleCommand("cite", Some(data.Node.ContentType.Cite))

  new ContentStyleCommand("br", Some(data.Node.ContentType.Br))

  new ContentStyleCommand("clear", None)



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
