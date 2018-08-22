package doc

import client.Client.ViewMessage
import model.range.IntRange
import model.{cursor, range}
import undoer.Undoer

case class DocTransaction(
  transaction: model.transaction.Node,
  mode: Option[model.mode.Node],
  subCodeMode: Option[model.mode.Content.CodeInside] = None,

  unfoldBefore: Set[cursor.Node] = Set.empty,
  toggleBefore: Set[cursor.Node] = Set.empty,
  zoomAfter: Option[cursor.Node] = None,

  undoType: Option[Undoer.Type] = None,
  tryMergeDeletes: Boolean = false,
  tryMergeInsertOfDeleteRange: Option[range.Node] = None,

  extra: Option[DocTransaction] = None, // this is used to add an undo item in the undo tree

  viewUpdated: Boolean = false,
  editorUpdated: Boolean = false,
  viewMessagesAfter: Seq[ViewMessage] = Seq.empty,
  viewMessagesBefore: Seq[ViewMessage] = Seq.empty) {
  def nonTransactional: Boolean = {
    transaction.isEmpty && mode.isEmpty && unfoldBefore.isEmpty && toggleBefore.isEmpty && zoomAfter.isEmpty && extra.isEmpty
  }

}

object DocTransaction {
  val empty = DocTransaction(Seq.empty, None)
  def apply(a: model.mode.Node): DocTransaction = DocTransaction(Seq.empty, Some(a))


  def message(a: ViewMessage): DocTransaction = {
    DocTransaction(Seq.empty, None, viewMessagesAfter = Seq(a))
  }
}

