package command

import client.Client
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model.data.Unicode
import model.range.IntRange

abstract class Command {

  def shownInCommandList: Boolean = true
  def category: String
  val description: String
  def hardcodeKeys: Seq[KeySeq] = Seq.empty
  def defaultKeys: Seq[KeySeq]
  def emptyAsFalse: Boolean = false

  def repeatable: Boolean = false
  def needsChar: Boolean = false
  def needsMotion: Boolean = false

  // TODO user keymap
  def keyLevel(c: KeySeq): Int = {
    if (defaultKeys.contains(c)) {
      1
    } else if (hardcodeKeys.contains(c)) {
      0
    } else {
      -1
    }
  }
  def keys:  Seq[KeySeq] = defaultKeys ++ hardcodeKeys // TODO key maps
  def available(a: DocState, commandState: CommandState): Boolean = available(a)
  protected def available(a: DocState): Boolean = throw new NotImplementedError()
  def action(a: DocState, count: Int, commandState: CommandState, key: Option[KeySeq]): DocTransaction = action(a, count)
  protected def action(a: DocState, count: Int): DocTransaction = throw new NotImplementedError()
  def actionOnGrapheme(a: DocState, char: Unicode, count: Int): DocTransaction = throw new NotImplementedError()
  def actionOnMotion(a: DocState, count: Int, to: IntRange): DocTransaction = throw new NotImplementedError()
}

