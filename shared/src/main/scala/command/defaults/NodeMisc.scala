package command.defaults

import client.Client.ViewMessage
import command.{CommandCategory, CommandInterface}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model._
import model.data.{Content, Text}
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer

class NodeMisc extends CommandCategory("node: misc") {

  new TextualCommand {
    override val description: String = "copy node link"
    override protected def available(a: DocState): Boolean = a.isContent
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      DocTransaction.message(ViewMessage.CopyToClipboard(a.node(a.asContent).refOfThis()))
    }
  }



  new TextualCommand {
    override val description: String = "reset content to: code"
    override protected def available(a: DocState): Boolean = a.isRich
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asContent
      DocTransaction(
        Seq(operation.Node.Replace(cur, data.Content.Code(data.Unicode(a.node(cur).rich.toPlain), ""))),
        Some(a.copyContentMode(mode.Content.CodeNormal(false))))
    }
  }

  new TextualCommand {
    override val description: String = "reset content to: rich text"
    override protected def available(a: DocState): Boolean = a.isCode
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (cur, code) = a.asCode
      val rich = data.Rich(if (code.unicode.isEmpty) Seq.empty else Seq(Text.Code(code.unicode)))
      DocTransaction(
        Seq(operation.Node.Replace(cur, data.Content.Rich(rich))),
        Some(a.copyContentMode(Content.Rich(rich).defaultMode(enableModal))))
    }
  }



  class ContentStyleCommand(desc: String, to: Option[data.Node.ContentType]) extends TextualCommand {
    override val description: String = s"content style: $desc"
    override protected def available(a: DocState): Boolean = a.isContent
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.changeContentType(a.asContent, to)
    }
  }

  for (i <- 2 to 6) {
    new ContentStyleCommand(s"heading $i, h$i", Some(data.Node.ContentType.Heading(i))) {
      override protected def available(a: DocState): Boolean = if (a.isContent) {
        if (i == 1) {
          true
        } else {
          val cur = a.asContent
          if (cur == cursor.Node.root) {
            false
          } else {
            a.node(cursor.Node.parent(cur)).heading match {
              case Some(j) =>  i == j + 1
              case None => true
            }
          }
        }
      } else {
        false
      }
    }
  }

  // evil char to affect sorting!
  new ContentStyleCommand(s"\u200Bheading 1, h1, article", Some(data.Node.ContentType.Heading(1)))

  new ContentStyleCommand("cite", Some(data.Node.ContentType.Cite))

  new ContentStyleCommand("hr", Some(data.Node.ContentType.Hr))

  new ContentStyleCommand("clear", None)

  class ChildrenStyleCommand(desc: String, to: Option[data.Node.ChildrenType]) extends TextualCommand {
    override val description: String = s"children style: $desc"
    override protected def available(a: DocState): Boolean = a.isContent
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asContent
      DocTransaction(
        Seq(operation.Node.AttributeChange(cur, data.Node.ChildrenType, to)),
        a.mode)
    }
  }

  new ChildrenStyleCommand("paragraphs", Some(data.Node.ChildrenType.Paragraphs))
  new ChildrenStyleCommand("ordered list, ol", Some(data.Node.ChildrenType.OrderedList))
  new ChildrenStyleCommand("unordered list, ul", Some(data.Node.ChildrenType.UnorderedList))
  new ChildrenStyleCommand("dash list, dl", Some(data.Node.ChildrenType.DashList))



  new TextualCommand {
    override val description: String = "insert rendered Markdown bellow (commonmark)"

    override protected def available(a: DocState): Boolean = a.isCode && model.parseFromCommonMarkMarkdown != null

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asContent
      val str = a.node(cur).content.asInstanceOf[Content.Code].unicode.str
      val node = model.parseFromCommonMarkMarkdown(str)
      DocTransaction(Seq(operation.Node.Insert(cursor.Node.moveBy(cur, 1), Seq(node))), None)
    }
  }
}
