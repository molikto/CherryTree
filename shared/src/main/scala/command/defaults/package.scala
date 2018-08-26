package command

import doc.{DocState, DocTransaction}
import model.{cursor, data, mode, operation, range}
import model.range.IntRange
import register.Registerable

package object defaults {

  def deleteNodeRange(a: DocState,
    commandState: CommandInterface,
    rr: range.Node,
    enableModal: Boolean,
    goUp: Boolean = false): DocTransaction = {
    val parent = a.node(rr.parent)
    val r = rr.copy(childs = IntRange(rr.childs.start, rr.childs.until min parent.childs.size))
    commandState.yank(Registerable.Node(a.node(rr), from = Some(rr), needsClone = false), isDelete = true)
    DocTransaction(Seq(operation.Node.Delete(r)), {
      val (nowPos, toPos) =
//        if (goUp) {
//          a.mover().visualUp(rr.start).
//        } else {
//        }
      if (a.node.get(r.until).isDefined) {
        (r.until, r.start)
      } else if (r.childs.start > 0) {
        val p = r.parent :+ (r.childs.start - 1)
        (p, p)
      } else {
        (r.parent, r.parent)
      }
      Some(model.mode.Node.Content(toPos, a.node(nowPos).content.defaultMode(enableModal)))
    }, tryMergeDeletes = true)
  }

  def deleteRichNormalRange(a: DocState, commandState: CommandInterface, pos: cursor.Node, r: IntRange, insert: Boolean, noHistory: Boolean = false): DocTransaction = {
    val rich = a.rich(pos)
    if (!noHistory) {
      commandState.yank(Registerable.Text(rich.copyTextualRange(r)), isDelete = true)
    }
    operation.Rich.deleteTextualRange(rich, r) match {
      case Some((a, b, c)) =>
        DocTransaction(
          a.map(r => operation.Node.rich(pos, r)),
          Some(mode.Node.Content(pos,
            if (insert)
              mode.Content.RichInsert(if (c == 0) b.start else b.until)
            else
              mode.Content.RichNormal(b)
          ))
        )
      case None => DocTransaction.empty
    }
  }


  private[defaults] def insertPointAfter(a: DocState, pos: cursor.Node): (cursor.Node, Option[data.Node.ContentType]) = {
    val mover = a.mover()
    val node = a.node(pos)
    if (pos == a.zoom || (node.isHeading && !a.viewAsFolded(pos))) {
      (pos :+ 0, None)
    } else {
      (mover.firstChild(pos).getOrElse(mover.nextOver(pos)), if (node.isHeading) node.attribute(data.Node.ContentType) else None)
    }
  }

}
