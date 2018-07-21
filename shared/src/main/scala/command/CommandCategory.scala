package command

import client.Client
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model.data.SpecialChar
import settings.Settings

import scala.collection.mutable.ArrayBuffer

class CommandCategory(val name: String) extends Settings {

  val commands = new ArrayBuffer[command.Command]()

  abstract class Command extends command.Command {
    commands.append(this)
    override def category: String = name
  }


  trait NeedsCharCommand extends Command {
    override def needsChar: Boolean = true
    override def action(a: DocState, count: Int): DocTransaction = throw new IllegalArgumentException("Not need this method")
  }

  abstract class DeliCommand(deli: SpecialChar.Delimitation) extends Command {
    override def defaultKeys: Seq[KeySeq] = delimitationCodePoints.get(deli.start) match {
      case Some(a) => Seq(a.toChar.toString)
      case None => Seq.empty
    }
  }

  trait MotionCommand extends Command {
    override def available(a: DocState): Boolean = a.isRichNormalOrVisual
  }

  trait OverrideCommand extends Command {
    def defaultKeys: Seq[KeySeq] = Seq.empty
    override def shownInCommandList: Boolean = false
  }

}
