package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface}
import command.Key._
import doc.{DocState, DocTransaction}
import model.cursor.Node
import model.range.IntRange
import model.{cursor, data, operation, range}
import settings.Settings

import scala.collection.mutable.ArrayBuffer

class NodeMove(settings: Settings) extends CommandCategory(settings,"node: move") {


  abstract class MoveCommand extends  Command {
    override def available(a: DocState): Boolean = a.isSingle
    def targetTo(mover: cursor.Node.Mover, node: cursor.Node): Option[cursor.Node]
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val mm = a.asSingle
      DocTransaction(targetTo(a.mover(), mm).map(n =>
        operation.Node.Move(range.Node(mm), n)
      ).toSeq, None)
    }
  }

  abstract class IndentCommand extends  Command {
    override def available(a: DocState): Boolean = a.mode.isDefined
    def targetTo(a: DocState, node: range.Node): Option[cursor.Node]


    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      def act(r: range.Node) = {
        val target = targetTo(a, r)
        val trans = target.toSeq.flatMap(to =>  {
           changeHeadingLevel(a, r, model.cursor.Node.parent(to)) :+ operation.Node.Move(r, to)
        })
        (target, trans)
      }

      val res = a.mode.get match {
        case v: model.mode.Node.Visual =>
          val r = a.asNodeVisual.minimalRange
            if (r.isDefined) act(r.get) else (None, Seq.empty)
        case c@model.mode.Node.Content(at, _) => if (at == a.zoom) (None, Seq.empty) else act(range.Node(at))
      }
      DocTransaction(res._2, None, unfoldBefore = res._1.map(a => model.cursor.Node.parent(a.to)).toSet)
    }
  }
  new IndentCommand {
    override val description: String = "unindent the node"
    override def defaultKeys: Seq[KeySeq] = Seq(Shift + Tab, Ctrl + "h", "<")
    override def targetTo(a: DocState, node: range.Node): Option[cursor.Node] = {
      val mover = a.mover()
      mover.parent(node.start).flatMap(p => {
        mover.parent(p).map(pp => pp :+ (p.last + 1))
      })
    }

  }
  new IndentCommand {
    override val description: String = "indent the node"
    override def defaultKeys: Seq[KeySeq] = Seq(Tab, Ctrl + "l", ">")

    override def available(a: DocState): Boolean = super.available(a)

    override def targetTo(a: DocState, node: range.Node): Option[cursor.Node] = {
      val mover = a.mover()
      mover.previous(node.start).map(a => a :+ mover.size(a))
    }
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
    override def available(a: DocState): Boolean = a.isSingle && a.asSingle != cursor.Node.root
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val mm = a.asSingle
      val n = a.node(mm)
      if (n.childs.isEmpty) {
        DocTransaction.empty
      } else {
        val ran = range.Node(mm, IntRange(0, n.childs.size))
        val to = cursor.Node.moveBy(mm, 1)
        changeHeadingLevel(a, ran, mm) :+ ran
        DocTransaction(Seq(
          operation.Node.Move(ran, to)
        ), None)
      }
    }
  }
}
