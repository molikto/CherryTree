package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface}
import command.Key._
import doc.{DocState, DocTransaction}
import model.range.IntRange
import model.{cursor, operation, range}

class NodeMove extends CommandCategory("node: move") {


  // LATER
  //unindent-row	<
  //indent-row	>

  abstract class MoveCommand extends  Command {
    override def available(a: DocState): Boolean = a.isContent
    def targetTo(mover: cursor.Node.Mover, node: cursor.Node): Option[cursor.Node]
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val mm = a.asContent
      DocTransaction(targetTo(a.mover(), mm).map(n => operation.Node.Move(range.Node(mm), n)).toSeq, None)
    }
  }
  abstract class IndentCommand extends  Command {
    override def available(a: DocState): Boolean = a.mode match {
      case None => false
      case Some(model.mode.Node.Content(_, model.mode.Content.CodeInside(_, _))) => false
      case _ => true
    }
    def targetTo(mover: cursor.Node.Mover, node: range.Node): Option[cursor.Node]

    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      def act(r: range.Node) = targetTo(a.mover(), r).map(k => operation.Node.Move(r, k))
      val res = a.mode.get match {
        case v: model.mode.Node.Visual =>
          a.asNodeVisual.minimalRange.flatMap(k => act(k))
        case c@model.mode.Node.Content(at, _) => if (at == a.zoom) None else act(range.Node(at))
      }
      DocTransaction(res.toSeq, None, unfoldBefore = res.map(a => model.cursor.Node.parent(a.to)).toSet)
    }
  }
  new IndentCommand {
    override val description: String = "unindent the node"
    override def defaultKeys: Seq[KeySeq] = Seq(Shift + Tab, Ctrl + "h", "<<")
    override def targetTo(mover: cursor.Node.Mover, node: range.Node): Option[cursor.Node] =
      mover.parent(node.start).flatMap(p => {
        mover.parent(p).map(pp => pp :+ (p.last + 1))
      })

  }
  new IndentCommand {
    override val description: String = "indent the node"
    override def defaultKeys: Seq[KeySeq] = Seq(Tab, Ctrl + "l", ">>")
    override def targetTo(mover: cursor.Node.Mover, node: range.Node): Option[cursor.Node] =
      mover.previous(node.start).map(a => a :+ mover.size(a))
  }
  new MoveCommand {
    override val description: String = "swap with next sibling"


    override def defaultKeys: Seq[KeySeq] = Seq(Ctrl + "j", shiftMod(Down))
    override def targetTo(mover: cursor.Node.Mover, node: cursor.Node): Option[cursor.Node] =
      mover.next(node).map(k => mover.nextOver(k))
  }
  new MoveCommand {
    override val description: String = "swap with previous sibling"
    override def defaultKeys: Seq[KeySeq] = Seq(Ctrl + "k", shiftMod(Up))
    override def targetTo(mover: cursor.Node.Mover, node: cursor.Node): Option[cursor.Node] =
      mover.previous(node)
  }

  new TextualCommand {
    override val description: String = "unwrap"
    override def available(a: DocState): Boolean = a.isContent
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val mm = a.asContent
      val n = a.node(mm)
      if (n.childs.isEmpty) {
        DocTransaction.empty
      } else {
        DocTransaction(Seq(
          operation.Node.Move(range.Node(mm, IntRange(0, n.childs.size)), cursor.Node.moveBy(mm, 1))
        ), None)
      }
    }
  }
}
