package command.defaults

import command.{CommandCategory, CommandInterface, CommandInterfaceAvailable, Motion}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Unicode
import settings.Settings

class Search(settings: Settings) extends CommandCategory(settings,"search & pattern matches") {

  new Command {
    override val description: String = "search forward"
    override val defaultKeys: Seq[KeySeq] = Seq("/")
    override val hardcodeKeys: Seq[KeySeq] = Seq(ModKey + "f")
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def clearOnConflict: Boolean = true
    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val isModF = hardcodeKeys.headOption == key
      commandState.startSearch(!isModF, if (isModF) 0 else 1)
      DocTransaction.empty
    }
  }

  new Command {
    override val description: String = "search backward"
    override def modalOnly: Boolean = true
    override def defaultKeys: Seq[KeySeq] = Seq("?")
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def clearOnConflict: Boolean = true
    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      commandState.startSearch(true, -1)
      DocTransaction.empty
    }
  }

  new Command {
    override val description: String = "repeat last search"
    override def defaultKeys: Seq[KeySeq] = Seq("n")
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def clearOnConflict: Boolean = true
    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      commandState.repeatLastSearch(false)
      DocTransaction.empty
    }
  }

  new Command {
    override val description: String = "repeat last search, in opposite direction"
    override def defaultKeys: Seq[KeySeq] = Seq("N")
    override def modalOnly: Boolean = true
    override def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = true
    override def clearOnConflict: Boolean = true
    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      commandState.repeatLastSearch(true)
      DocTransaction.empty
    }
  }
}
