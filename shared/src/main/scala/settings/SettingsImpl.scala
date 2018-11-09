package settings

import client.LocalStorage
import command.Key
import model.data.{SpecialChar, Unicode}

trait SettingsImpl extends Settings {

  def localStorage: LocalStorage


  var enableModal: Boolean = localStorage.get("settings.enable_modal").getOrElse("false") == "true"
  /**
    * these are settings??
    */
  var delimitationSettings = Seq(
    (SpecialChar.StrikeThrough, Unicode("~"), Unicode("~")),
    (SpecialChar.Code, Unicode("`"), Unicode("`")),
    (SpecialChar.Strong, Unicode("**"), Unicode("**")),
    (SpecialChar.LaTeX, Unicode("$"), Unicode("$")),
    (SpecialChar.Link, Unicode("["), Unicode("]")),
    (SpecialChar.HashTag, Unicode("#"), Unicode("#")),
    (SpecialChar.HashDef, Unicode("##"), Unicode("##")),
    (SpecialChar.Emphasis, Unicode("*"), Unicode("*"))
  )

  var delimitationGraphemes: SpecialKeySettings = delimitationSettings.flatMap(a => Seq(a._1.start -> a._2, a._1.end -> a._3)).toMap

  var additionalKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty

  var removedDefaultKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty
}
