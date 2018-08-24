package command.defaults

import command.{CommandCategory, CommandInterface, Key, Motion}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Unicode
import model.{cursor, operation, range}
import model.range.IntRange
import register.Registerable

class NodeDelete extends CommandCategory("node: delete") {

  private val message = ". if the deleted node is immediately inserted back to the document within 5 seconds," +
    " this is considered a node movement," +
    " and causes less conflicts when editing collaboratively"

  def deleteNodeRange(a: DocState, commandState: CommandInterface, rr: range.Node): DocTransaction = {

    val parent = a.node(rr.parent)
    val r = rr.copy(childs = IntRange(rr.childs.start, rr.childs.until min parent.childs.size))
    commandState.yank(Registerable.Node(a.node(rr), from = Some(rr), needsClone = false), isDelete = true)
    DocTransaction(Seq(operation.Node.Delete(r)), {
      val (nowPos, toPos) = if (a.node.get(r.until).isDefined) {
        (r.until, r.start)
      } else if (r.childs.start > 0) {
        val p = r.parent :+ (r.childs.start - 1)
        (p, p)
      } else {
        (r.parent, r.parent)
      }
      Some(model.mode.Node.Content(toPos, a.node(nowPos).content.defaultNormalMode()))
    }, tryMergeDeletes = true)
  }


  new Command {
    override val description: String = "delete current node" + message
    override val defaultKeys: Seq[KeySeq] = Seq("dd", shiftMod(Backspace)) // siblings not lines
    override def available(a: DocState): Boolean = a.isNormal

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val r = a.asNormal._1
      if (r == a.zoom) DocTransaction.empty
      else deleteNodeRange(a, commandState, model.range.Node(a.asNormal._1)) // we don't allow multiple deletes for now!
    }
  }

  new Command {
    override val description: String = "delete selected nodes" + message
    override val defaultKeys: Seq[KeySeq] = Seq("d", "D", "x", "X", Delete)
    override def available(a: DocState): Boolean = a.isNodeVisual

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.mode match {
        case Some(v@model.mode.Node.Visual(_, _)) =>
          v.minimalRange.map(r => deleteNodeRange(a, commandState, r)).getOrElse(DocTransaction.empty)
        case _ => throw new IllegalArgumentException("Invalid command")
      }
    }
  }
}
