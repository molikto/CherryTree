package settings

import command.Key
import command.Key.Grapheme
import model.data.{SpecialChar, Unicode}

trait Settings {

  val enableModal: Boolean = true
  /**
    * these are settings??
    */
  val delimitationSettings = Seq(
    (SpecialChar.StrikeThrough, Unicode("~"), Unicode("~")),
    (SpecialChar.Code, Unicode("`"), Unicode("`")),
    (SpecialChar.Strong, Unicode("**"), Unicode("**")),
    (SpecialChar.LaTeX, Unicode("$"), Unicode("$")),
    (SpecialChar.Link, Unicode("["), Unicode("]")),
    (SpecialChar.HashTag, Unicode("#"), Unicode("#")),
    (SpecialChar.HashDef, Unicode("##"), Unicode("##")),
    (SpecialChar.Emphasis, Unicode("*"), Unicode("*"))
  )

  val delimitationGraphemes: SpecialKeySettings = delimitationSettings.flatMap(a => Seq(a._1.start -> a._2, a._1.end -> a._3)).toMap

  def additionalKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty

  def removedDefaultKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty
}
