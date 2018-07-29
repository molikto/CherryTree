package command.defaults

import command.{CommandCategory, CommandInterface, Motion}
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model.data.Unicode
import model.{data, mode, operation}

class NodeMenu extends CommandCategory("node menu") {

  new Command {
    override val description: String = "(DEBUG TEMP) convert node to code"
    override protected def available(a: DocState): Boolean = a.isNormal
    override def defaultKeys: Seq[KeySeq] = Seq("n")

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val cur = a.asNormal._1
      DocTransaction(Seq(operation.Node.Replace(cur, data.Content.Code(Unicode(s"""function findSequence(goal) {
                                                                                  |  function find(start, history) {
                                                                                  |    if (start == goal)
                                                                                  |      return history;
                                                                                  |    else if (start > goal)
                                                                                  |      return null;
                                                                                  |    else
                                                                                  |      return find(start + 5, "(" + history + " + 5)") ||
                                                                                  |             find(start * 3, "(" + history + " * 3)");
                                                                                  |  }
                                                                                  |  return find(1, "1");
                                                                                  |}
                                                                                  |
       """.stripMargin), "source/text/javascript"))),
        Some(mode.Node.Content(cur, mode.Content.CodeNormal))
      )
    }
  }
}
