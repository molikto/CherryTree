package command

import client.Client
import command.Key.{Grapheme, KeySeq}
import model.ClientState

abstract class Command {

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
  def available(a: ClientState): Boolean
  def action(a: ClientState, count: Int): Client.Update
  def actionOnGrapheme(a: ClientState, char: Grapheme, count: Int): Client.Update = throw new NotImplementedError()
}

