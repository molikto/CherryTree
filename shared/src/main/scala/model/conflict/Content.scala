package model.conflict


abstract sealed class Content()
object Content {
  case class CodeContent(u: Unicode) extends Content
  case class CodeLang(l: String) extends Content
  case class Rich(u: Rich) extends Content
}
