package command.defaults

import client.Client.ViewMessage
import client.{ContentTypeRule, InputRule, ParentChildrenTypeRule}
import command.{CommandCategory, CommandInterface}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model._
import model.data.{Content, Text}
import model.range.IntRange

import scala.collection.mutable.ArrayBuffer

class NodeStyle extends CommandCategory("node: format") {



  new TextualCommand {
    override val description: String = "reset content to: code" // we don't want to reset root node to code, this will make document title display ugly...
    override protected def available(a: DocState): Boolean = a.isRichNonSub && a.asContent != model.cursor.Node.root
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



  class ContentStyleCommand(desc: String, ir: Seq[String], to: Option[data.Node.ContentType]) extends TextualCommand {
    override val description: String = s"content style: $desc"
    override protected def available(a: DocState): Boolean = a.isContent
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.changeContentType(a.asContent, to)
    }

    override val inputRule: Seq[InputRule] = to.toSeq.flatMap(t => ir.map(i => new ContentTypeRule(i, t)))
  }


  // the reason is commands is textual declare ordered, so it's the input rule append order
  for (i <- (2 to 6).reverse) {
    new ContentStyleCommand(s"heading $i, h$i", Seq((0 until i).map(_ => "#").mkString("") + " "), Some(data.Node.ContentType.Heading(i))) {
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
  new ContentStyleCommand(s"\u200Bheading 1, h1, article", Seq("# "), Some(data.Node.ContentType.Heading(1)))

  new ContentStyleCommand("cite", Seq("> "), Some(data.Node.ContentType.Cite))

  new ContentStyleCommand("hr", Seq("___"), Some(data.Node.ContentType.Hr))

  new ContentStyleCommand("clear", Seq.empty, None)

  class ChildrenStyleCommand(desc: String, ir: Seq[String], to: Option[data.Node.ChildrenType]) extends TextualCommand {
    override val description: String = s"list style: $desc"
    override protected def available(a: DocState): Boolean = a.isContent
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asContent
      DocTransaction(
        Seq(operation.Node.AttributeChange(cur, data.Node.ChildrenType, to)),
        a.mode)
    }
    override val inputRule: Seq[InputRule] = to.toSeq.flatMap(t => ir.map(i => new ParentChildrenTypeRule(i, t)))
  }

  new ChildrenStyleCommand("paragraphs", Seq.empty, Some(data.Node.ChildrenType.Paragraphs))
  new ChildrenStyleCommand("ordered list, ol", Seq("1. "), Some(data.Node.ChildrenType.OrderedList))
  new ChildrenStyleCommand("unordered list, ul", Seq("+ ", "* "), Some(data.Node.ChildrenType.UnorderedList))
  new ChildrenStyleCommand("dash list, dl", Seq("- "), Some(data.Node.ChildrenType.DashList))



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
