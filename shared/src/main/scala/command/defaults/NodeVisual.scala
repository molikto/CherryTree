package command.defaults

import client.Client
import command.CommandCategory
import command.Key._
import doc.{DocState, DocTransaction}
import model.mode

class NodeVisual extends CommandCategory("node visual") {

  // DIFFERENCE going from node visual to content visual is NOT possible
  // CTRL-V   CTRL-V       start highlighting blockwise   }  highlighted text
  // v_CTRL-V CTRL-V       highlight blockwise or stop highlighting
  // gv       gv           start highlighting on previous visual area

  new Command {
    override def description: String = "enter/exit node visual mode"
    override val defaultKeys: Seq[KeySeq] = Seq("V") // DIFFERENCE merged two command
    override def available(a: DocState): Boolean = a.mode match {
      case Some(m) => m match {
        case model.mode.Node.Content(_, mm) => mm.isNormalOrVisual
        case model.mode.Node.Visual(_, _) => true
        case _ => false
      }
      case None => false
    }
    override def action(a: DocState, count: Int): DocTransaction = a.mode match {
      case Some(m) => m match {
        case model.mode.Node.Content(at, mm) if mm.isNormalOrVisual =>
          DocTransaction.mode(mode.Node.Visual(at, at))
        case model.mode.Node.Visual(_, move) =>
          DocTransaction.mode(model.data.Node.defaultNormalMode(a.node, move))
        case _ => throw new IllegalArgumentException("Wrong branch")
      }
      case None => throw new IllegalArgumentException("Wrong branch")
    }

  }


  new Command {
    override def description: String = "swap movable and fixed cursor"
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: DocState): Boolean = a.isNodeVisual
    override def action(a: DocState, count: Int): DocTransaction = DocTransaction.mode(a.asNodeVisual.swap)
  }

}