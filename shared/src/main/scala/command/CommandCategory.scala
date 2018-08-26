package command

import client.Client
import command.Key.{Grapheme, KeySeq}
import doc.{DocState, DocTransaction}
import model.data.{SpecialChar, Unicode}
import settings.Settings
import Key._

import scala.collection.mutable.ArrayBuffer


class CommandCategory(val name: String) extends Settings {

  val commands = new ArrayBuffer[command.Command]()

  trait Command extends command.Command {
    commands.append(this)
    override def category: String = name
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

    private val graphemes =  Seq(deli.start, deli.end).flatMap(delimitationGraphemes.get).distinct
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
