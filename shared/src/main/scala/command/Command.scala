package command

import client.Client
import command.Key.{Grapheme, KeySeq}
import doc.{DocState, DocTransaction}

abstract class Command {

  def shownInCommandList: Boolean = true
  def repeatable: Boolean = false
  def category: String
  def description: String
  def hardcodeKeys: Seq[KeySeq] = Seq.empty
  def defaultKeys: Seq[KeySeq]

  def needsChar: Boolean = false

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
  def action(a: DocState, count: Int, commandState: CommandState): DocTransaction = action(a, count)
  protected def available(a: DocState): Boolean
  protected def action(a: DocState, count: Int): DocTransaction
  def actionOnGrapheme(a: DocState, char: Grapheme, count: Int): DocTransaction = throw new NotImplementedError()
}

