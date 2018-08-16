package view

import command.Key
import model.{cursor, operation}
import model.data.Unicode
import model.range.IntRange
import monix.reactive.Observable

trait EditorInterface {
  def codeEdit(op: Seq[operation.Unicode]): Unit
  def exitCodeEdit(): Unit
  def disableStateUpdate: Boolean
  def disableStateUpdate_=(a: Boolean): Unit
  def flushes: Observable[Unit]
  def flush(): Unit
  def onInsertRichTextAndViewUpdated(unicode: Unicode)
  def onAttributeModified(cur: cursor.Node, range: IntRange, url: Unicode, title: Unicode)
  def onLaTeXModified(cur: cursor.Node, range: IntRange, uni: Seq[operation.Unicode])
  def onExternalPastePlain(a: Unicode)
  def onKeyDown(k: Key): Boolean
}
