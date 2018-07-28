package doc

import undoer.Undoer

case class DocUpdate(
  root: model.data.Node,
  transaction: model.transaction.Node,
  mode: Option[model.mode.Node],
  ty: Undoer.Type,
  viewUpdated: Boolean = false)

