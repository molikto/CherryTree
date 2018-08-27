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



  new Command {
    override val description: String = "delete current node" + message
    override val defaultKeys: Seq[KeySeq] = Seq("dd", shiftMod(Backspace)) // siblings not lines
    override def available(a: DocState): Boolean = a.isContent

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val r = a.asContent
      deleteNodeRange(a, commandState, model.range.Node(r), enableModal) // we don't allow multiple deletes for now!
    }
  }

  new Command {
    override val description: String = "delete selected nodes" + message
    override val defaultKeys: Seq[KeySeq] = Seq("d", "D", "x", "X", Delete)
    override def available(a: DocState): Boolean = a.isNodeVisual

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      a.mode match {
        case Some(v@model.mode.Node.Visual(_, _)) =>
          v.minimalRange.map(r => deleteNodeRange(a, commandState, r, enableModal)).getOrElse(DocTransaction.empty)
        case _ => throw new IllegalArgumentException("Invalid command")
      }
    }
  }
}
