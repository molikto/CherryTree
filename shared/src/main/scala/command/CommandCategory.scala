package command

import client.Client
import command.Key.{Grapheme, KeySeq}
import doc.{DocState, DocTransaction}
import model.data.{SpecialChar, Unicode}
import settings.Settings
import Key._
import client.Client.ViewMessage
import model.cursor
import model.cursor.Node

import scala.collection.mutable.ArrayBuffer


class CommandCategory(val settings: Settings, val name: String) {




  val commands = new ArrayBuffer[command.Command]()

  trait Command extends command.Command {
    if (settings.enableModal || !modalOnly) {
      commands.append(this)
    }
    override def category: String = name
  }


  /**
    * CTRL-M and <CR>)
    * _     N  _            down N-1 lines, on the first non-blank character
    */
  // LATER these
  trait NodeMotionCommand extends Command {

    override def showInCommandMenu(modal: Boolean): Boolean = false
    override def repeatable: Boolean = true
    def move(data: DocState, a: cursor.Node): Option[cursor.Node] = None
    def message: Option[ViewMessage] = None
    def stayInVisual: Boolean = false

    override def available(a: DocState): Boolean = a.mode.nonEmpty

    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      def act(r: cursor.Node): cursor.Node = (0 until count).foldLeft(r) {(r, _) => move(a, r).getOrElse(r)}
      var exitModal = false
      val modeTo = a.mode match {
        case Some(m) => m match {
          case v@model.mode.Node.Visual(_, mm) =>
            if (!settings.enableModal && !stayInVisual) exitModal = true
            v.copy(move = act(mm))
          case kkk@model.mode.Node.Content(n, cc) =>
            model.data.Node.defaultMode(a.node, act(n), settings.enableModal)
        }
        case None => throw new IllegalArgumentException("Not allowed")
      }
      val messages = if (exitModal) Seq(ViewMessage.ExitVisual)  else Seq.empty
      DocTransaction(Seq.empty, Some(modeTo), viewMessagesBefore = message.toSeq, viewMessagesAfter = messages)
    }
  }


  trait RichMotionCommand extends Command with command.RichMotion {
    override def modalOnly: Boolean = true
    override def showInCommandMenu(modal: Boolean): Boolean = false
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = a.isRichNormalOrNoneEmptyVisual || commandState.needsMotion
  }


  abstract class UpDownCommand(val isUp: Boolean, val isBlockWise: Boolean, val hasShift: Boolean) extends NodeMotionCommand {
    override def repeatable: Boolean = isBlockWise
    override def stayInVisual: Boolean = hasShift
    override protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      if (a.isNodeVisual) {
        super.action(a, commandState, count)
      } else {
        DocTransaction.message(Client.ViewMessage.VisualUpDownMotion(isUp, if (isBlockWise) count else -1, hasShift))
      }
    }

    override def move(data: DocState, a: Node): Option[Node] = if (isUp) data.mover().visualUp(a) else data.mover().visualDown(a)
  }
  trait TextualCommand extends Command {
    override def defaultKeys: Seq[KeySeq] = Seq.empty
  }

  trait NeedsCharCommand extends Command {
    override def needsChar: Boolean = true
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new IllegalArgumentException("Not need this method")
  }

  abstract class DeliCommand(deli: SpecialChar.Delimitation) extends Command {
    // currently these cannot be changed, you can change delimiters though


    override def strong: Boolean = true

    private def graphemes =  Seq(deli.start, deli.end).flatMap(settings.delimitationGraphemes.get).filter(_.nonEmpty).distinct
    private val systemKeys: Seq[KeySeq] =
      if (deli == SpecialChar.Emphasis) Seq(ModKey + "i", ModKey + "y")
      else if (deli == SpecialChar.Strong) Seq(ModKey + "b")
      else Seq.empty

    override def maybeInsertModeGrapheme(u: Unicode): Boolean = graphemes.exists(a => a.startsWith(u))

    override def defaultKeys: Seq[KeySeq] = Seq.empty
    override def hardcodeKeys: Seq[KeySeq] =
      graphemes.map(a => a.str : KeySeq) ++ systemKeys
  }

  trait OverrideCommand extends Command with NonConfigurableCommand {
  }

}
