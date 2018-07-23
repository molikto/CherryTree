package settings

import command.Key
import command.Key.Grapheme
import model.data.SpecialChar

trait Settings {



  /**
    * these are settings??
    */
  val delimitationSettings = Seq(
    (SpecialChar.StrikeThrough, Grapheme("~"), Grapheme("~")),
    (SpecialChar.Code, Grapheme("`"), Grapheme("`")),
    (SpecialChar.Strong, Grapheme("#"), Grapheme("#")),
    (SpecialChar.LaTeX, Grapheme("&"), Grapheme("&")),
    (SpecialChar.Link, Grapheme("["), Grapheme("]")),
    (SpecialChar.Emphasis, Grapheme("*"), Grapheme("*"))
  )

  val delimitationCodePoints: Map[SpecialChar, Grapheme] = delimitationSettings.flatMap(a => Seq(a._1.start -> a._2, a._1.end -> a._3)).toMap

  def additionalKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty

  def removedDefaultKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty
}
