package command.defaults

import client.Client
import command.CommandCollector
import command.Key._
import doc.{DocState, DocTransaction}
import model.range.IntRange
import model.{cursor, mode, operation}

trait Delete extends CommandCollector {


  // LATER
  // J     N  J            join N-1 lines (delete <EOL>s)
  //v_J      {visual}J    join the highlighted lines
  //gJ    N  gJ           like "J", but without inserting spaces
  //v_gJ     {visual}gJ   like "{visual}J", but without inserting spaces
  //:d    :[range]d [x]   delete [range] lines [into register x]

  private def deleteRichNormalRange(a: DocState, pos: cursor.Node, r: IntRange): DocTransaction = {
    val rich = a.rich(pos)
    val (ifs, soc, roc) = rich.infoAndSingleSpecials(r)
    val ds = r.minusOrderedInside(soc)
    def deleteRanges(i: Seq[IntRange]): DocTransaction = {
      val remaining = IntRange(0, rich.size).minusOrderedInside(i)
      val posTo = if (remaining.isEmpty) {
        IntRange(0, 0) // all deleted
      } else {
        // for all remaining bits
        val p = remaining.find(_.until > r.start).map(_.start max r.start).map(a => a).getOrElse(remaining.last.until - 1)
        val tempPos = rich.info(p).atomicRange
        tempPos.moveByOrZeroZero(-i.filter(_.start < tempPos.start).map(_.size).sum)
      }
      DocTransaction(
        Seq(operation.Node.Content(pos,
          operation.Content.Rich(operation.Rich.deleteNoneOverlappingOrderedRanges(i))
        )),
        Some(mode.Node.Content(pos, mode.Content.RichNormal(posTo)))
      )
    }
    if (ds.isEmpty) {
      if (ifs.forall(_.isStart)) {
        deleteRanges((roc ++ Seq(r)).sortBy(_.start))
      } else if (ifs.forall(_.isEndOrAttributeTagOrContent)) {
        deleteRanges((roc ++ Seq(r)).sortBy(_.start))
      } else {
        DocTransaction.empty
      }
    } else {
      deleteRanges(ds)
    }
  }

  private def deleteNodeRange(a: DocState, rr: model.range.Node): DocTransaction = {
    val parent = a.node(rr.parent)
    val r = rr.copy(childs = IntRange(rr.childs.start, rr.childs.until min parent.childs.size))
    DocTransaction(Seq(operation.Node.Delete(r)), {
      val (nowPos, toPos) = if (a.node.get(r.until).isDefined) {
        (r.until, r.start)
      } else if (r.childs.start > 0) {
        val p = r.parent :+ (r.childs.start - 1)
        (p, p)
      } else {
        (r.parent, r.parent)
      }
      Some(model.mode.Node.Content(toPos, a.node(nowPos).content.defaultNormalMode()))
    })
  }

  val deleteAfterVisual: Command = new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("d", "D", "x", "X", Delete)
    override def available(a: DocState): Boolean = a.isVisual
    override def action(a: DocState, count: Int): DocTransaction = a.mode match {
      case Some(v@model.mode.Node.Visual(_, _)) =>
        v.minimalRange.map(r => deleteNodeRange(a, r)).getOrElse(DocTransaction.empty)
      case Some(model.mode.Node.Content(pos, v@model.mode.Content.RichVisual(_, _))) =>
        deleteRichNormalRange(a, pos, v.merged)
      case _ => throw new IllegalArgumentException("Invalid command")
    }
  }

  val delete: Command = new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("x", Delete)
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val (pos, rich, normal) = a.asRichNormal
      val r = normal.range
      val fr = (1 until count).foldLeft(r) {(r, _) => rich.moveRightAtomic(r) }
      deleteRichNormalRange(a, pos, r.merge(fr))
    }
  }

  val deleteBefore: Command = new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("X")
    override def available(a: DocState): Boolean = a.isRichNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val (pos, rich, normal) = a.asRichNormal
      val r = normal.range
      val rr = rich.moveLeftAtomic(r)
      val fr = (1 until count).foldLeft(rr) {(r, _) => rich.moveLeftAtomic(r) }
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

  val deleteSiblings: Command = new Command {
    override val defaultKeys: Seq[KeySeq] = Seq("dd") // siblings not lines
    override def available(a: DocState): Boolean = a.isNormal
    override def action(a: DocState, count: Int): DocTransaction = {
      val r = a.asNormal._1
      if (r == cursor.Node.root) DocTransaction.empty
      else deleteNodeRange(a, model.range.Node(a.asNormal._1, count))
    }
  }

  val deleteUntilEnd: Command = new Command {
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
