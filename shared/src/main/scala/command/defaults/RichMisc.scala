package command.defaults

import client.Client.ViewMessage
import command.{CommandCategory, CommandInterface}
import doc.{DocState, DocTransaction}
import model.data.{SpecialChar, Text, Unicode}
import model.range.IntRange
import model.{data, mode, operation}

class RichMisc extends CommandCategory("rich text: misc") {


  new TextualCommand {
    override val description: String = "insert image"
    override protected def available(a: DocState): Boolean = a.isRichNormalOrInsert
    override protected def action(state: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (cur, rich, pos, until) = state.asRichNormalOrInsert
      val deli = SpecialChar.Image.wrap()
      val (mo, range) =
        if (until >= 0) {
          val r = IntRange(until, until + deli.size)
          (mode.Content.RichNormal(r), r)
        } else {
          (mode.Content.RichInsert(pos + deli.size), IntRange(pos, pos + deli.size))
        }
      DocTransaction(
        Seq(operation.Node.rich(cur, operation.Rich.insert(pos, deli))),
        Some(state.copyContentMode(mo)),
        viewMessagesAfter = Seq(ViewMessage.ShowUrlAndTitleAttributeEditor(cur, range, Text.Image(Unicode.empty)))
      )
    }
  }
}
