package doc

import model.{cursor, range}
import undoer.Undoer

case class DocTransaction(
  transaction: model.transaction.Node,
  mode: Option[model.mode.Node],
  handyAppliedResult: Option[model.data.Node] = None,
  // TODO make use of them
  unfoldBefore: Set[cursor.Node] = Set.empty,
  toggleBefore: Set[cursor.Node] = Set.empty,
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

