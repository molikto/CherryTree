package command

import doc.{DocState, DocTransaction}
import model.{cursor, mode, operation}
import model.range.IntRange

package object defaults {

  private[defaults] def deleteRichNormalRange(a: DocState, pos: cursor.Node, r: IntRange, insert: Boolean): DocTransaction = {
    val rich = a.rich(pos)
    operation.Rich.deleteTextualRange(rich, r) match {
      case Some((a, b, c)) =>
        DocTransaction(
          a.map(r => operation.Node.Content(pos,
            operation.Content.Rich(r)))
          ,
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

}
