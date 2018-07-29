package doc

import model.{cursor, range}
import undoer.Undoer

case class DocTransaction(
  transaction: model.transaction.Node,
  mode: Option[model.mode.Node],
  // TODO make use of them
  handyAppliedResult: Option[model.data.Node] = None,
  unfoldBefore: Seq[cursor.Node] = Seq.empty,
  foldBefore: Seq[cursor.Node] = Seq.empty,
  undoType: Option[Undoer.Type] = None,
  tryMergeDeletes: Boolean = false,
  tryMergeInsertOfDeleteRange: Option[range.Node] = None,
  // TODO zoom options
  viewUpdated: Boolean = false) {
}
object DocTransaction {
  val empty = DocTransaction(Seq.empty, None)
  def mode(a: model.mode.Node) = DocTransaction(Seq.empty, Some(a))
}

