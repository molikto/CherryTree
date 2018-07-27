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
  def needsStuff = needsMotion || needsChar

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
  def available(a: DocState, commandState: CommandInterface): Boolean = available(a) && !commandState.needsMotion
  protected def available(a: DocState): Boolean = throw new NotImplementedError(description)
  def action(a: DocState,
    count: Int,
    commandState: CommandInterface,
    key: Option[KeySeq],
    grapheme: Option[Unicode],
    motion: Option[Motion]
  ): DocTransaction = action(a, commandState, count)
  protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new NotImplementedError(description)
}


trait SideEffectingCommand extends Command  {

}
