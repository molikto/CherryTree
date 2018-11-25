package model.conflict

import model.data.CodeType
import model.{conflict, data}


abstract sealed class Content()
object Content {
  case class CodeContent(u: Unicode) extends Content
  case class CodeLang(l: CodeType) extends Content
  case class Rich(u: conflict.Rich) extends Content
}
