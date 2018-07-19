package command

import client.Client
import command.Key.KeySeq
import model.ClientState
import model.data.SpecialChar
import settings.Settings

import scala.collection.mutable.ArrayBuffer

trait CommandCollector extends Settings {

  protected val commands = new ArrayBuffer[command.Command]()

  def registerCommand(c: command.Command): Unit = commands.append(c)


  abstract class Command extends command.Command {
    registerCommand(this)
  }


  trait NeedsCharCommand extends Command {
    override def needsChar: Boolean = true
    override def action(a: ClientState, count: Int): Client.Update = throw new IllegalArgumentException("Not need this method")
  }

  abstract class DeliCommand(deli: SpecialChar.Delimitation) extends Command {
    override def defaultKeys: Seq[KeySeq] = delimitationCodePoints.get(deli.start) match {
      case Some(a) => Seq(a.toChar.toString)
      case None => Seq.empty
    }
  }

  trait MotionCommand extends Command {
    override def available(a: ClientState): Boolean = a.isRichNormalOrVisual
  }

  trait OverrideCommand extends Command {
    def defaultKeys: Seq[KeySeq] = Seq.empty
  }

}
