package doc

import model.cursor
import undoer.Undoer

case class DocTransaction(
  transaction: model.transaction.Node,
  mode: Option[model.mode.Node],
  // TODO make use of them
  unfoldBefore: Seq[cursor.Node] = Seq.empty,
  foldBefore: Seq[cursor.Node] = Seq.empty,
  undoType: Option[Undoer.Type] = None,
  // TODO zoom options
  viewUpdated: Boolean = false) {
}
object DocTransaction {
  val empty = DocTransaction(Seq.empty, None)
  def mode(a: model.mode.Node) = DocTransaction(Seq.empty, Some(a))
}

