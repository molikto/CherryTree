package command

import client.Client
import command.Key.{Grapheme, KeySeq}
import doc.{DocState, DocTransaction}
import model.data.SpecialChar
import settings.Settings
import Key._

import scala.collection.mutable.ArrayBuffer

class CommandCategory(val name: String) extends Settings {

  val commands = new ArrayBuffer[command.Command]()

  abstract class Command extends command.Command {
    commands.append(this)
    override def category: String = name
  }

  abstract class TextualCommand extends Command {
    override def defaultKeys: Seq[KeySeq] = Seq.empty
  }


  trait NeedsCharCommand extends Command {
    override def needsChar: Boolean = true
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new IllegalArgumentException("Not need this method")
  }

  abstract class DeliCommand(deli: SpecialChar.Delimitation) extends Command {
    // currently these cannot be changed, you can change delimiters though

    private val systemKeys: Seq[KeySeq] =
      if (deli == SpecialChar.Emphasis) Seq(ModKey + "i", ModKey + "y")
      else if (deli == SpecialChar.Strong) Seq(ModKey + "b")
      else Seq.empty

    override def defaultKeys: Seq[KeySeq] = Seq.empty
    override def hardcodeKeys: Seq[KeySeq] =
      Seq(deli.start, deli.end).flatMap(delimitationGraphemes.get).distinct.map(a => Grapheme(a) : KeySeq) ++ systemKeys


  }

  trait OverrideCommand extends Command {
    def defaultKeys: Seq[KeySeq] = Seq.empty
  }

}
