package command.defaults

import client.Client
import command.{CommandCategory, CommandInterface, Motion}
import command.Key._
import doc.{DocState, DocTransaction}
import model.data.{Atom, Unicode}
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


  new Command {
    override def repeatable: Boolean = true
    override val description: String = "delete under cursor, and more after if has N"
    override val defaultKeys: Seq[KeySeq] = Seq("x", Delete)
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (pos, rich, normal) = a.asRichNormal
      if (rich.isEmpty) return DocTransaction.empty
      val r = normal.range
      val fr = (1 until count).foldLeft(r) {(r, _) => if (r.until == rich.size) r else rich.rangeAfter(r) }
      deleteRichNormalRange(a, commandState,pos, r.merge(fr), insert = false)
    }

  }

  new Command {
    override def repeatable: Boolean = true
    override val description: String = "delete before cursor, and more if has N"
    override val defaultKeys: Seq[KeySeq] = Seq("X")
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (pos, rich, normal) = a.asRichNormal
      if (rich.isEmpty) return DocTransaction.empty
      val r = normal.range
      val rr = rich.rangeBefore(r)
      val fr = (1 until count).foldLeft(rr) {(r, _) => rich.rangeBefore(r) }
      deleteRichNormalRange(a, commandState,pos, rr.merge(fr), insert = false)
    }
  }

  new Command {
    override val description: String = "delete selected text"
    override val defaultKeys: Seq[KeySeq] = Seq("d", "D", "x", "X", Delete)
    override def available(a: DocState): Boolean = a.isRichVisual
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = a.mode match {
      case Some(model.mode.Node.Content(pos, v@model.mode.Content.RichVisual(_, _))) =>
        deleteRichNormalRange(a, commandState,pos, v.merged, insert = false)
      case _ => throw new IllegalArgumentException("Invalid command")
    }
  }


  new Command {
    override val description: String = "delete range selected by motion"
    override def needsMotion: Boolean = true
    override val defaultKeys: Seq[KeySeq] = Seq("d")
    override protected def available(a: DocState): Boolean = a.isRichNormal

    override def action(a: DocState, count: Int, commandState: CommandInterface, key: Option[KeySeq], grapheme: Option[Unicode], motion: Option[Motion]): DocTransaction = {
      val (cur, rich, normal) = a.asRichNormal
      if (rich.isEmpty) return DocTransaction.empty
      motion.flatMap(m => {
        m.act(commandState, rich, count, normal.range, grapheme).map(r => {
          deleteRichNormalRange(a, commandState,cur, r, insert = false)
        })
      }).getOrElse(DocTransaction.empty)
    }
  }

  new Command {
    override val description: String = "delete text cursor until text end"
    override def defaultKeys: Seq[KeySeq] = Seq("D")
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = {
      val (c, rich, normal) = a.asRichNormal
//      val deleteLines = if (c == cursor.Node.root || count <= 1) {
//        Seq.empty
//      } else {
//        val p = c.dropRight(1)
//        Seq(operation.Node.Delete(model.range.Node(p, IntRange(c.last + 1, (c.last + count) min p.size))))
//      }
      val deleteFirstLine = deleteRichNormalRange(a, commandState,c, IntRange(normal.range.start, rich.size), insert = false)
      deleteFirstLine
      //deleteFirstLine.copy(transaction = deleteFirstLine.transaction ++ deleteLines)
    }
  }
}
