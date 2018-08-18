package doc

import model.cursor
import model.range.IntRange
import undoer.Undoer

case class DocUpdate(
  to: DocState,
  from: Seq[(DocState, model.operation.Node, DocState)],
  foldsBefore: Map[cursor.Node, Boolean],
  fromUser: Boolean, // if this is true, then when mode is updated, we perform scroll to mode etc.
  viewUpdated: Boolean,
  editorUpdated: Boolean) {
}

