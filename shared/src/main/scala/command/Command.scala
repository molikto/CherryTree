package command

import client.{Client, InputRule}
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
  def textCommand: Seq[String] = Seq.empty

  def repeatable: Boolean = false
  def needsChar: Boolean = false
  def needsMotion: Boolean = false
  def documentOnly: Boolean = false
  def needsStuff = needsMotion || needsChar || documentOnly

  def actDoubleClick: Boolean = false

  def actTripleClick: Boolean = false

  val inputRule: Seq[InputRule] = Seq.empty

  def priority(c: KeySeq): Int = if (defaultKeys.contains(c)) {
      1
  } else {
      0
  }

  def showInCommandMenu(modal: Boolean): Boolean = if (modal) keys.isEmpty else  keys.forall(_.exists(_.isSimpleGrapheme))

  def keys:  Seq[KeySeq] = defaultKeys ++ hardcodeKeys // TODO key maps
  def available(a: DocState, commandState: CommandInterfaceAvailable): Boolean = available(a) && !commandState.needsMotion
  protected def available(a: DocState): Boolean =throw new IllegalArgumentException(s"not implemented $description")
  def action(a: DocState,
    count: Int,
    commandState: CommandInterface,
    key: Option[KeySeq],
    grapheme: Option[Unicode],
    motion: Option[Motion]
  ): DocTransaction = action(a, commandState, count)
  protected def action(a: DocState, commandState: CommandInterface, count: Int): DocTransaction = throw new IllegalArgumentException(description)
}


trait SideEffectingCommand extends Command  {

}
