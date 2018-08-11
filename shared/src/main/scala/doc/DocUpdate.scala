package doc

import model.cursor
import model.range.IntRange
import undoer.Undoer

case class DocUpdate(
  root: model.data.Node,
  transaction: Either[Seq[(model.operation.Node, cursor.Node)], cursor.Node],
  mode: Option[model.mode.Node],
  zoomAfter: model.cursor.Node,
  foldsBefore: Map[cursor.Node, Boolean],
  fromUser: Boolean, // if this is true, then when mode is updated, we perform scroll to mode etc.
  viewUpdated: Boolean) {
}

