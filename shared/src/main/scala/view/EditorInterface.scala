package view

import command.Key
import model.{cursor, operation}
import model.data.{CodeType, Unicode}
import model.range.IntRange
import monix.reactive.Observable

trait EditorInterface {

  def onCodeSubModeAndEditorUpdated(str: String, a: Int): Unit
  def onCodeEditAndEditorUpdated(op: Seq[operation.Unicode]): Unit
  def onCodeTypeChangeAndEditorUpdated(to: CodeType): Unit
  def exitCodeEdit(): Unit
  def disableStateUpdate: Boolean
  def disableStateUpdate_=(a: Boolean): Unit
  def flushes: Observable[Unit]
  def flush(): Unit
  def onInsertRichTextAndViewUpdated(unicode: Unicode)
  def onAttributeModified(cur: cursor.Node, range: IntRange, url: Unicode, title: Unicode)
  def onInlineModifiedAndEditorUpdated(cur: cursor.Node, range: IntRange, uni: Seq[operation.Unicode])
  def onInlineCodeTypeChangedAndEditorUpdated(cur: cursor.Node, range: IntRange, ty: CodeType)
  def onInlineSubModeAndEditorUpdated(str: String, a: Int): Unit
  def onExternalPasteInRichEditor(a: Unicode)
  def onKeyDown(k: Key): Boolean
}
