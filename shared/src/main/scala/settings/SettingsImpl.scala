package settings

import client.LocalStorage
import command.Key
import model.data.{SpecialChar, Unicode}

trait SettingsImpl extends Settings {

  def localStorage: LocalStorage


  var enableModal: Boolean = localStorage.get("settings.enable_modal").getOrElse("true") == "true"

  def writeEnableModal(): Unit = {
    localStorage.set("settings.enable_modal", enableModal.toString)
  }
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
    (SpecialChar.Sub, Unicode("_"), Unicode("_")),
    (SpecialChar.Underline, Unicode("_"), Unicode("_")),
    (SpecialChar.Sup, Unicode("^"), Unicode("^")),
    (SpecialChar.HashDef, Unicode("##"), Unicode("##")),
    (SpecialChar.Emphasis, Unicode("*"), Unicode("*")),
    (SpecialChar.SpanClass, Unicode("<"), Unicode(">"))
  )

  var delimitationGraphemes: SpecialKeySettings = delimitationSettings.flatMap(a => Seq(a._1.start -> a._2, a._1.end -> a._3)).toMap

  var disableDelmitationKeys = Set(SpecialChar.Sub, SpecialChar.Underline, SpecialChar.SpanClass)

  var additionalKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty

  var removedDefaultKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty
}
