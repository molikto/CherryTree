package view

import command.Key
import model.data.Unicode
import monix.reactive.Observable

trait EditorInterface {

  def disableStateUpdate: Boolean
  def disableStateUpdate_=(a: Boolean): Unit
  def flushes: Observable[Unit]
  def flush(): Unit
  def onInsertRichTextAndViewUpdated(unicode: Unicode)
  def onKeyDown(k: Key): Boolean
}