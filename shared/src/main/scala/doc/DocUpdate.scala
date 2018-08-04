package doc

import model.cursor
import model.range.IntRange
import undoer.Undoer

case class DocUpdate(
  root: model.data.Node,
  transaction: model.transaction.Node,
  mode: Option[model.mode.Node],
  folds: Map[cursor.Node, Boolean],
  fromUser: Boolean,
  viewUpdated: Boolean) {
}

