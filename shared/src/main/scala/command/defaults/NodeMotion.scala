package command.defaults

import client.Client
import client.Client.ViewMessage
import command.{CommandCategory, CommandInterface, Motion}
import command.Key._
import doc.{DocState, DocTransaction}
import model.cursor
import model.cursor.Node
import model.data.Unicode
import settings.Settings

class NodeMotion(settings: Settings) extends CommandCategory(settings, "node: motion") {

  abstract class Command extends super.Command {
    override def modalOnly: Boolean = true
  }

  new UpDownCommand(true, true, false) {
    override def modalOnly: Boolean = true
    override val description: String = "move up"
    // DIFFERENCE we always go to first char now
    // DIFFERENCE k and - is merged
    override val defaultKeys: Seq[KeySeq] = Seq("k", "-")
  }

  new UpDownCommand(false, true, false) {
    override def modalOnly: Boolean = true
    override val description: String = "move down"
    override val defaultKeys: Seq[KeySeq] = Seq("j", "+")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().visualDown(a)
  }

  val parent: Command = new Command with NodeMotionCommand {
    override val description: String = "move to parent"
    override val defaultKeys: Seq[KeySeq] = Seq("gp")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().parent(a)
  }
  val nextSibling: Command = new Command with NodeMotionCommand {
    override val description: String = "move to next sibling"
    override val defaultKeys: Seq[KeySeq] = Seq("}")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().next(a)
  }
  val previousSibling: Command = new Command with NodeMotionCommand {
    override val description: String = "move to previous sibling"
    override val defaultKeys: Seq[KeySeq] = Seq("{")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().previous(a)
  }
  val visibleBeginning: Command = new Command with NodeMotionCommand {
    override val description: String = "move to top of viewport"
    override val defaultKeys: Seq[KeySeq] = Seq("gg")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = Some(data.zoom)
    override def message: Option[ViewMessage] = Some(ViewMessage.ScrollToTop)
  }
  val visibleEnd: Command = new Command with NodeMotionCommand {
    override val description: String = "move to bottom of viewport"
    override val defaultKeys: Seq[KeySeq] = Seq("G")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = Some(data.mover().visualBottom(data.zoom))
    override def message: Option[ViewMessage] = Some(ViewMessage.ScrollToBottom)
  }


  // not implemented for not understand what should it behave
  // * N%    N  %            goto line N percentage down in the file; N must be
  // * given, otherwise it is the % command

  // LATER screen related not implemented
  //        gk    N  gk           up N screen lines (differs from "k" when line wraps)
  //        gj    N  gj           down N screen lines (differs from "j" when line wraps)


}
