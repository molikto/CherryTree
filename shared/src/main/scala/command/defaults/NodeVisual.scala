package command.defaults

import client.Client
import command.CommandCollector
import command.Key._
import model.{ClientState, mode}

trait NodeVisual extends CommandCollector {

  // DIFFERENCE going from node visual to content visual is NOT possible
  // CTRL-V   CTRL-V       start highlighting blockwise   }  highlighted text
  // v_CTRL-V CTRL-V       highlight blockwise or stop highlighting
  // gv       gv           start highlighting on previous visual area

  new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("V") // DIFFERENCE merged two command
    override def available(a: ClientState): Boolean = a.mode match {
      case Some(m) => m match {
        case model.mode.Node.Content(_, mm) => mm.isNormalOrVisual
        case model.mode.Node.Visual(_, _) => true
        case _ => false
      }
      case None => false
    }
    override def action(a: ClientState, count: Int): Client.Update = a.mode match {
      case Some(m) => m match {
        case model.mode.Node.Content(at, mm) if mm.isNormalOrVisual =>
          Client.Update.mode(mode.Node.Visual(at, at))
        case model.mode.Node.Visual(_, move) =>
          Client.Update.mode(model.data.Node.defaultNormalMode(a.node, move))
        case _ => throw new IllegalArgumentException("Wrong branch")
      }
      case None => throw new IllegalArgumentException("Wrong branch")
    }
  }


  new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("o")
    override def available(a: ClientState): Boolean = a.isNodeVisual
    override def action(a: ClientState, count: Int): Client.Update = Client.Update.mode(a.asNodeVisual.swap)
  }

}
