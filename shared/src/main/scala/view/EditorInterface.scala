package view

import command.Key
import doc.DocTransaction
import model.cursor.Node
import model.{cursor, operation}
import model.data.{CodeType, Unicode}
import model.mode.Content.CodeInside
import model.range.IntRange
import monix.reactive.Observable

trait SourceEditInterface {
  def onChangeAndEditorUpdated(op: Seq[operation.Unicode], inside: CodeInside): Unit
  def onCodeTypeChangeAndEditorUpdated(to: CodeType): Unit
  def exitSubMode(): Unit
  def onSourceEditorCommandBuffer(a: String): Unit
  def onSourceEditorUndo(): Unit
  def onSourceEditorRedo(): Unit
}

trait EditorInterface extends SourceEditInterface {
  def onVisualMode(mouseFirstContent: Node, node: Node): Unit

  def refreshMode(): Unit
  def focusOn(cur: Node, range: Option[IntRange], viewUpdated: Boolean): Boolean
  def disableRemoteStateUpdate(disable: Boolean, forMouse: Boolean): Unit
  def flushes: Observable[Unit]
  def flush(): Unit
  def onInsertRichTextAndViewUpdated(start: Int, end: Int, unicode: Unicode, domInsertion: Int)
  def onAttributeModified(url: Unicode, title: Unicode)
  def onExternalPasteInRichEditor(a: Unicode)
  def onKeyDown(k: Key): Boolean
}
