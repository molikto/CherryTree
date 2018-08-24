package command

import client.Client
import command.Key.KeySeq
import doc.{DocState, DocTransaction}
import model.data.Unicode
import model.range.IntRange

trait NonConfigurableCommand extends Command {
  override def keyConfigurable: Boolean = false
  override def defaultKeys: Seq[KeySeq] = Seq.empty
}

abstract class Command {
  def category: String
  def strong: Boolean = false
  def keyConfigurable: Boolean = true
  val description: String
  def hardcodeKeys: Seq[KeySeq] = Seq.empty
  def defaultKeys: Seq[KeySeq]
  def emptyAsFalseInInsertMode: Boolean = false
  def maybeInsertModeGrapheme(u: Unicode): Boolean = false

  def repeatable: Boolean = false
  def needsChar: Boolean = false
  def needsMotion: Boolean = false
  def needsStuff = needsMotion || needsChar

  
  def priority: Int = 0
  // TODO user keymap
  def keyLevel(c: KeySeq): Int = {
    ( if (defaultKeys.contains(c)) {
      1
    } else if (hardcodeKeys.contains(c)) {
      0
    } else {
      -1
    }) + priority
  }
  def keys:  Seq[KeySeq] = defaultKeys ++ hardcodeKeys // TODO key maps
  def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = available(a) && !commandState.needsMotion
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
