package command.defaults

import client.Client
import client.Client.ViewMessage
import command.{CommandCategory, CommandInterface}
import command.Key._
import doc.{DocState, DocTransaction}
import model.cursor

class NodeMotion extends CommandCategory("move among notes") {



  /**
    * CTRL-M and <CR>)
    * _     N  _            down N-1 lines, on the first non-blank character
    */
  // LATER these
  abstract class NodeMotionCommand extends Command {
    override def repeatable: Boolean = true
    def move(data: DocState, a: cursor.Node): Option[cursor.Node]
    def message: Option[ViewMessage] = None

    override def available(a: DocState): Boolean = a.mode match {
      case Some(m) => m match {
        case model.mode.Node.Visual(fix, move) => true
        case model.mode.Node.Content(n, cc) => cc match {
          case model.mode.Content.RichNormal(n) => true
          case model.mode.Content.CodeNormal => true
          case _ => false
        }
      }
      case None => false
    }

    final override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      def act(r: cursor.Node): cursor.Node = (0 until count).foldLeft(r) {(r, _) => move(a, r).getOrElse(r)}
      DocTransaction(Seq.empty, Some(a.mode match {
        case Some(m) => m match {
          case v@model.mode.Node.Visual(_, mm) => v.copy(move = act(mm))
          case kkk@model.mode.Node.Content(n, cc) => cc match {
            case _: model.mode.Content.Normal =>
              model.data.Node.defaultNormalMode(a.node, act(n))
            case _ => throw new MatchError("Not allowed")
          }
        }
        case None => throw new MatchError("Not allowed")
      }), viewMessagesBefore = message.toSeq)
    }
  }
  /*
  val up: Command = new NodeMotionCommand {
    override val description: String = "move up"
    // DIFFERENCE we always go to first char now
    // DIFFERENCE k and - is merged
    override val defaultKeys: Seq[KeySeq] = Seq("k", "-", Up)
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().visualUp(a)

  }
  val down: Command = new NodeMotionCommand {
    override val description: String = "move down"
    override val defaultKeys: Seq[KeySeq] = Seq("j", "+", Down)
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().visualDown(a)
  }
  */
  val parent: Command = new NodeMotionCommand {
    override val description: String = "move to parent"
    override val defaultKeys: Seq[KeySeq] = Seq("gp")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().parent(a)
  }
  val nextSibling: Command = new NodeMotionCommand {
    override val description: String = "move to next sibling"
    override val defaultKeys: Seq[KeySeq] = Seq("}")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().next(a)
  }
  val previousSibling: Command = new NodeMotionCommand {
    override val description: String = "move to previous sibling"
    override val defaultKeys: Seq[KeySeq] = Seq("{")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = data.mover().previous(a)
  }
  val visibleBeginning: Command = new NodeMotionCommand {
    override val description: String = "move to top of viewport"
    override val defaultKeys: Seq[KeySeq] = Seq("gg")
    override def move(data: DocState, a: cursor.Node): Option[cursor.Node] = Some(data.zoom)
    override def message: Option[ViewMessage] = Some(ViewMessage.ScrollToTop)
  }
  val visibleEnd: Command = new NodeMotionCommand {
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
