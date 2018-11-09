package settings

import command.Key
import command.Key.Grapheme
import model.data.{SpecialChar, Unicode}

trait Settings {

  def enableModal: Boolean

  def delimitationSettings: Seq[(model.data.SpecialChar.Delimitation, Unicode, Unicode)]

  def delimitationGraphemes: SpecialKeySettings

  def additionalKeyMaps: Map[String, Seq[Key.KeySeq]]

  def removedDefaultKeyMaps: Map[String, Seq[Key.KeySeq]]
}
