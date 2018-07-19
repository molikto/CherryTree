package settings

import command.Key
import model.data.SpecialChar

trait Settings {



  /**
    * these are settings??
    */
  val delimitationSettings = Seq(
    (SpecialChar.StrikeThrough, '~'.toInt, '~'.toInt),
    (SpecialChar.Code, '`'.toInt, '`'.toInt),
    (SpecialChar.Strong, '#'.toInt, '#'.toInt),
    (SpecialChar.LaTeX, '&'.toInt, '&'.toInt),
    (SpecialChar.Link, '['.toInt, ']'.toInt),
    (SpecialChar.Emphasis, '*'.toInt, '*'.toInt)
  )

  val delimitationCodePoints: Map[SpecialChar, Int] = delimitationSettings.flatMap(a => Seq(a._1.start -> a._2, a._1.end -> a._2)).toMap

  def additionalKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty

  def removedDefaultKeyMaps: Map[String, Seq[Key.KeySeq]] = Map.empty
}
