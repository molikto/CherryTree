package command.defaults

import client.Client.ViewMessage
import client.{CreateListTypeRule, InputRule, NodeTypeRule}
import command.{CommandCategory, CommandInterface, Motion}
import command.Key.{KeySeq, Shift}
import doc.{DocState, DocTransaction}
import model._
import model.data.{CodeType, Content, Text, Unicode}
import model.range.IntRange
import settings.Settings
import command.Key._
import model.data.Node.HeadingLevel
import model.operation.Node.AttributeChange

import scala.collection.mutable.ArrayBuffer

class NodeStyle(settings: Settings) extends CommandCategory(settings,"node: format") {



  new TextualCommand {
    override val description: String = "reset content to: code" // we don't want to reset root node to code, this will make document title display ugly...
    override protected def available(a: DocState): Boolean = a.isRichNonSub && a.asSingle != model.cursor.Node.root
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asSingle
      DocTransaction(
        Seq(operation.Node.Replace(cur, data.Content.Code(data.Unicode(a.node(cur).rich.toPlain), CodeType.Empty))),
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
        Some(a.copyContentMode(Content.Rich(rich).defaultMode(settings.enableModal))))
    }
  }



  class NodeTypeCommand(desc: String, ir: Seq[String], to: data.NodeType) extends TextualCommand {
    override val description: String = s"node type: $desc"
    override protected def available(a: DocState): Boolean = a.isSingle && a.canChangeTo(a.asSingle, to)

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.changeNodeTypeHeadingLevel(a.asSingle, to)
    }

    override val inputRule: Seq[InputRule] = ir.map(i => new NodeTypeRule(i, to))
  }


  for (i <- 1 to 6) {
    new Command {
      override val description: String = s"change heading level to $i"
      override def defaultKeys: Seq[KeySeq] = Seq.empty

      override protected def available(a: DocState): Boolean = a.isSingle && a.node(a.asSingle).isHeading

      override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
        DocTransaction(Seq(AttributeChange(a.asSingle, HeadingLevel, Some(i))), None)
      }
    }
  }


  new NodeTypeCommand("outline", Seq.empty, data.NodeType.Outline)

  new NodeTypeCommand("article", Seq.empty, data.NodeType.Article)

  new NodeTypeCommand("heading", Seq("= "), data.NodeType.Heading)

  new NodeTypeCommand("block, blockquote", Seq("> "), data.NodeType.Block)

  new NodeTypeCommand("divider, hr", Seq("___"), data.NodeType.Divider)

  new NodeTypeCommand("paragraph", Seq.empty, data.NodeType.Paragraph)

  new NodeTypeCommand("list item, li", Seq.empty, data.NodeType.Li)

  new NodeTypeCommand("multi-paragraph list item", Seq.empty, data.NodeType.LiParagraphs)

  class ListStyleCommand(desc: String, ir: Seq[String], to: Option[data.Node.ListType]) extends TextualCommand {
    override val description: String = s"list style: $desc"
    override protected def available(a: DocState): Boolean = a.isSingle && a.nodeAndType(a.asSingle).isList
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asSingle
      DocTransaction(
        Seq(operation.Node.AttributeChange(cur, data.Node.ListType, to)),
        a.mode)
    }
    override val inputRule: Seq[InputRule] = to.toSeq.flatMap(t => ir.map(i => new CreateListTypeRule(i, t)))
  }

  new ListStyleCommand("ordered list, ol", Seq("1. "), Some(data.Node.ListType.OrderedList))
  new ListStyleCommand("unordered list, ul", Seq("+ ", "* "), Some(data.Node.ListType.UnorderedList))
  new ListStyleCommand("dash list, dl", Seq("- "), Some(data.Node.ListType.DashList))



  new TextualCommand {
    override val description: String = "insert rendered Markdown bellow (commonmark)"

    override protected def available(a: DocState): Boolean = a.isCode && platform.parseFromCommonMarkMarkdown != null

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val cur = a.asContent
      val str = a.node(cur).content.asInstanceOf[Content.Code].unicode.str
      val node = platform.parseFromCommonMarkMarkdown(str)
      DocTransaction(Seq(operation.Node.Insert(cursor.Node.moveBy(cur, 1), Seq(node))), None)
    }
  }

  class PriorityCommand(override val description: String, key: String, map: Int => Int) extends Command {
    override def defaultKeys: Seq[KeySeq] = Seq(shiftMod(key))

    override protected def available(a: DocState): Boolean = a.mode.nonEmpty

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.mode.get match {
        case model.mode.Node.Content(node, _) =>
          val p = map(a.node(node).priority.getOrElse(0))
          DocTransaction(Seq(operation.Node.AttributeChange(node, data.Node.Priority, Some(p))), None)
        case v@model.mode.Node.Visual(_, _) =>
          val range = v.minimalRange.getOrElse(Seq.empty)
          DocTransaction(range.toSeq.map(node => {
            val p = map(a.node(node).priority.getOrElse(0))
            operation.Node.AttributeChange(node, data.Node.Priority, Some(p))
          }), None)
      }
    }

  }

  new PriorityCommand("increase priority by 1", "=", _ + 1)
  new PriorityCommand("decrease priority by 1", "-", _ - 1)

}
