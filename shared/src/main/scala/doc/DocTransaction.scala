package doc

import client.Client.ViewMessage
import model.{cursor, range}
import undoer.Undoer

case class DocTransaction(
  transaction: model.transaction.Node,
  mode: Option[model.mode.Node],
  handyAppliedResult: Option[model.data.Node] = None,

  unfoldBefore: Set[cursor.Node] = Set.empty,
  toggleBefore: Set[cursor.Node] = Set.empty,
  zoomAfter: Option[cursor.Node] = None,

  undoType: Option[Undoer.Type] = None,
  tryMergeDeletes: Boolean = false,
  tryMergeInsertOfDeleteRange: Option[range.Node] = None,

  // TODO zoom options
  viewUpdated: Boolean = false,
  viewMessagesAfter: Seq[ViewMessage] = Seq.empty,
  viewMessagesBefore: Seq[ViewMessage] = Seq.empty) {
  def nonTransactional: Boolean = {
    transaction.isEmpty && mode.isEmpty && unfoldBefore.isEmpty && toggleBefore.isEmpty && zoomAfter.isEmpty
  }
}

object DocTransaction {
  val empty = DocTransaction(Seq.empty, None)
  def apply(a: model.mode.Node): DocTransaction = DocTransaction(Seq.empty, Some(a))

  def message(a: ViewMessage): DocTransaction = {
    DocTransaction(Seq.empty, None, viewMessagesBefore = Seq(a))
  }
}

