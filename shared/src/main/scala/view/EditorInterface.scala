package view

import command.Key
import doc.DocTransaction
import model.{cursor, operation}
import model.data.{CodeType, Unicode}
import model.mode.Content.CodeInside
import model.range.IntRange
import monix.reactive.Observable


trait SourceEditInterface {
  def onChangeAndEditorUpdated(op: Seq[operation.Unicode], inside: CodeInside): Unit
  def onCodeTypeChangeAndEditorUpdated(to: CodeType): Unit
  def exitCodeEdit(): Unit
  def onSourceEditorCommandBuffer(a: String): Unit
  def onSourceEditorUndo(): Unit
  def onSourceEditorRedo(): Unit
}

trait EditorInterface extends SourceEditInterface {

  def disableStateUpdate: Boolean
  def disableStateUpdate_=(a: Boolean): Unit
  def flushes: Observable[Unit]
  def flush(): Unit
  def onInsertRichTextAndViewUpdated(unicode: Unicode)
  def onAttributeModified(url: Unicode, title: Unicode)
  def onExternalPasteInRichEditor(a: Unicode)
  def onKeyDown(k: Key): Boolean
}
