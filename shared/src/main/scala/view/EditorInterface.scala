package view

import command.Key
import model.data.Unicode
import monix.reactive.Observable

trait EditorInterface {
  def exitCodeEditMode(ops: Seq[model.operation.Unicode]): Unit


  def disableStateUpdate: Boolean
  def disableStateUpdate_=(a: Boolean): Unit
  def flushes: Observable[Unit]
  def flush(): Unit
  def onInsertRichTextAndViewUpdated(unicode: Unicode)
  def onExternalPastePlain(a: Unicode)
  def onKeyDown(k: Key): Boolean
}
