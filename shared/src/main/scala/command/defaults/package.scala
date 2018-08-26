package command

import doc.{DocState, DocTransaction}
import model.{cursor, data, mode, operation}
import model.range.IntRange
import register.Registerable

package object defaults {

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
