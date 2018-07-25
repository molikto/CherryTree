package command.defaults

import client.Client
import command.CommandCategory
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.Atom
import model.range.IntRange
import model.{cursor, mode, operation}

// TODO save to buffer
class RichDelete extends CommandCategory("delete text") {


  // LATER
  // J     N  J            join N-1 lines (delete <EOL>s)
  //v_J      {visual}J    join the highlighted lines
  //gJ    N  gJ           like "J", but without inserting spaces
  //v_gJ     {visual}gJ   like "{visual}J", but without inserting spaces
  //:d    :[range]d [x]   delete [range] lines [into register x]

  private def deleteRichNormalRange(a: DocState, pos: cursor.Node, r: IntRange): DocTransaction = {
    val rich = a.rich(pos)
    operation.Rich.deleteTextualRange(rich, r) match {
      case Some(a) =>
        DocTransaction(
          a._1.map(r => operation.Node.Content(pos,
            operation.Content.Rich(r)))
          ,
          Some(mode.Node.Content(pos, mode.Content.RichNormal(a._2)))
        )
      case None => DocTransaction.empty
    }
  }


  new Command {
    override val description: String = "delete selected text"
    override val defaultKeys: Seq[KeySeq] = Seq("d", "D", "x", "X", Delete)
    override def available(a: DocState): Boolean = a.isRichVisual
    override def action(a: DocState, count: Int): DocTransaction = a.mode match {
      case Some(model.mode.Node.Content(pos, v@model.mode.Content.RichVisual(_, _))) =>
        deleteRichNormalRange(a, pos, v.merged)
      case _ => throw new IllegalArgumentException("Invalid command")
    }
  }

  new Command {
    override def repeatable: Boolean = true
    override val description: String = "delete under cursor, and more after if has N"
    override val defaultKeys: Seq[KeySeq] = Seq("x", Delete)
    override def available(a: DocState): Boolean = a.isNonEmptyRichNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val (pos, rich, normal) = a.asRichNormal
      val r = normal.range
      val fr = (1 until count).foldLeft(r) {(r, _) => if (r.until == rich.size) r else rich.rangeAfter(r) }
      deleteRichNormalRange(a, pos, r.merge(fr))
    }

  }

  new Command {
    override def repeatable: Boolean = true
    override val description: String = "delete before cursor, and more if has N"
    override val defaultKeys: Seq[KeySeq] = Seq("X")
    override def available(a: DocState): Boolean = a.isNonEmptyRichNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val (pos, rich, normal) = a.asRichNormal
      val r = normal.range
      val rr = rich.rangeBefore(r)
      val fr = (1 until count).foldLeft(rr) {(r, _) => rich.rangeBefore(r) }
      deleteRichNormalRange(a, pos, rr.merge(fr))
    }
  }

  //      // TODO dw etc
  //      val deleteMotion: Command = new Command {
  //        override val defaultKeys: Seq[KeySeq] = Seq("d")
  //        override def available(a: ClientState): Boolean = a.isNormal
  //
  //        override def action(a: ClientState, count: Int): DocTransaction = {
  //          DocTransaction.empty
  //        }
  //      }

  new Command {
    override val description: String = "delete text cursor until text end"
    override def defaultKeys: Seq[KeySeq] = Seq("D")
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val (c, rich, normal) = a.asRichNormal
      val deleteLines = if (c == cursor.Node.root || count <= 1) {
        Seq.empty
      } else {
        val p = c.dropRight(1)
        val parent = a.node(p)
        Seq(operation.Node.Delete(model.range.Node(p, IntRange(c.last + 1, (c.last + count) min p.size))))
      }
      val deleteFirstLine = deleteRichNormalRange(a, c, IntRange(normal.range.start, rich.size))
      deleteFirstLine.copy(transaction = deleteFirstLine.transaction ++ deleteLines)
    }
  }
}
