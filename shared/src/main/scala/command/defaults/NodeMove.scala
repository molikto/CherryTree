package command.defaults

import client.Client
import command.CommandCollector
import command.Key._
import doc.{DocState, DocTransaction}
import model.{cursor, operation, range}

trait NodeMove extends CommandCollector {


  // LATER
  //unindent-row	<
  //indent-row	>

  abstract class MoveCommand extends  Command {
    override def available(a: DocState): Boolean = a.isNormal
    def targetTo(mover: cursor.Node.Mover, node: cursor.Node): Option[cursor.Node]
    override def action(a: DocState, count: Int): DocTransaction = {
      val mm = a.asNormal._1
      DocTransaction(targetTo(a.mover(), mm).map(n => operation.Node.Move(range.Node(mm), n)).toSeq, None)
    }
  }
  abstract class IndentCommand extends  Command {
    override def available(a: DocState): Boolean = a.mode.nonEmpty
    def targetTo(mover: cursor.Node.Mover, node: range.Node): Option[cursor.Node]

    override def action(a: DocState, count: Int): DocTransaction = {
      def act(r: range.Node) = targetTo(a.mover(), r).map(k => operation.Node.Move(r, k))
      val res = a.mode.get match {
        case v: model.mode.Node.Visual =>
          a.asNodeVisual.minimalRange.flatMap(k => act(k))
        case c@model.mode.Node.Content(at, _) => if (at == cursor.Node.root) None else act(range.Node(at))
      }
      DocTransaction(res.toSeq, None, unfoldBefore = res.toSeq.map(_.to))
    }
  }
  val unindent: Command = new IndentCommand {
    override def defaultKeys: Seq[KeySeq] = Seq(Shift + Tab, Ctrl + "h")
    override def targetTo(mover: cursor.Node.Mover, node: range.Node): Option[cursor.Node] =
      mover.parent(node.start).flatMap(p => {
        mover.parent(p).map(pp => pp :+ (p.last + 1))
      })
  }
  val indent: Command = new IndentCommand {
    override def defaultKeys: Seq[KeySeq] = Seq(Tab, Ctrl + "l")
    override def targetTo(mover: cursor.Node.Mover, node: range.Node): Option[cursor.Node] =
      mover.previous(node.start).map(a => a :+ mover.size(a))
  }
  val swapDown: Command = new MoveCommand {
    override def defaultKeys: Seq[KeySeq] = Seq(Ctrl + "j")
    override def targetTo(mover: cursor.Node.Mover, node: cursor.Node): Option[cursor.Node] =
      mover.next(node).map(k => mover.nextOver(k))
  }
  val swapUp: Command = new MoveCommand {
    override def defaultKeys: Seq[KeySeq] = Seq(Ctrl + "k")
    override def targetTo(mover: cursor.Node.Mover, node: cursor.Node): Option[cursor.Node] =
      mover.previous(node)
  }
}
