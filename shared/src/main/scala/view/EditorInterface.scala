package view

import command.Key
import doc.DocTransaction
import model.cursor.Node
import model.{cursor, mode, operation}
import model.data.{CodeType, Unicode}
import model.mode.Content.CodeInside
import model.range.IntRange
import monix.reactive.Observable
import register.RegisterInterface
import settings.Settings

trait SourceEditInterface extends RegisterInterface {
  def onChangeAndEditorUpdated(op: Seq[operation.Unicode], inside: CodeInside): Unit
  def onCodeTypeChangeAndEditorUpdated(to: CodeType): Unit
  def onExitSubMode(): Unit
  def onSourceEditorCommandBuffer(a: String): Unit
  def onSourceEditorUndo(): Unit
  def onSourceEditorRedo(): Unit
}

trait RichEditInterface {
  def onInsertRichTextAndViewUpdated(start: Int, end: Int, unicode: Unicode, toNormal: Boolean, posInDom: Int, mergeWithPrevious: Boolean): mode.Content.Rich
  def onAttributeModified(url: Unicode, title: Unicode)
  // returns if there is any data change
  def onDeleteCurrentSelectionAndStartInsert(): Boolean
}

trait EditorInterface extends SourceEditInterface with RichEditInterface with Settings {

  def onDoubleClick(): Unit

  def onVisualMode(mouseFirstContent: Node, node: Node): Unit
  def flushBeforeMouseDown()

  def onRefreshMode(): Unit
  def onMouseFocusOn(cur: Node, range: Option[IntRange], leftIsAnchor: Boolean, viewUpdated: Boolean): Boolean
  def disableRemoteStateUpdate(disable: Boolean, forMouse: Boolean): Unit
  def flushes: Observable[Unit]
  def onKeyDown(k: Key): Boolean

  def onExternalPasteInRichEditor(html: Option[String], plain: Option[String], ct: Option[String])
  // returns
  def onExternalCopyCut(isCut: Boolean): (Option[String], Option[String], Option[String])
}
