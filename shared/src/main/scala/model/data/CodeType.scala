package model.data

import boopickle.{PickleState, Pickler, UnpickleState}
import play.api.libs.json._


sealed abstract class CodeType(val str: String) {
  def codeType = str
  def displayNmae: String
  def codeMirror: String = if (codeType == "latex") "stex" else if (codeType == "html") "htmlmixed" else codeType
  def delimitation: SpecialChar.Delimitation = if (this == Embedded.LaTeX) {
    SpecialChar.LaTeX
  } else if (this == Embedded.HTML) {
    SpecialChar.HTML
  } else {
    null
  }
}

case class SourceCode(name: String) extends CodeType(s"source/$name") {
  override def codeType: String = name

  override def displayNmae: String = s"Source: ${CodeType.codeNameToDisplayName(name)}"
}

case object LaTeXMacro extends CodeType("latex-macro") {
  override def codeType: String = "latex"

  override def displayNmae: String = "LaTeX macro"
}
case class Embedded(name: String) extends CodeType(s"embedded/$name") {
  override def codeType: String = name
  override def displayNmae: String = s"Embedded: ${CodeType.codeNameToDisplayName(name)}"

}
object Embedded {
  val LaTeX = Embedded("latex")
  val HTML = Embedded("html")
}

object CodeType {


  case object Empty extends CodeType("") {
    override def displayNmae: String = "Undefined"
  }

  case object Plain extends CodeType("plain") {
    override def codeMirror: String = ""
    override def displayNmae: String = "Plain"
  }

  case class Other(name: String) extends CodeType(name) {
    override def displayNmae: String = name
  }

  def codeNameToDisplayName(str: String): String = {
    str match {
      case "latex" => "LaTeX"
      case "html" => "HTML"
      case "javascript" => "JavaScript"
      case "kotlin" => "Kotlin"
      case "markdown" => "Markdown"
      case a => a
    }
  }
  def parse(a: String): CodeType = {
    if (a.startsWith("source/")) {
      SourceCode(a.substring("source/".length))
    } else if (a == "latex-macro") {
      LaTeXMacro
    } else if (a == "plain") {
      CodeType.Plain
    } else if (a == "") {
      CodeType.Empty
    } else if (a.startsWith("embedded/")) {
      Embedded(a.substring("embedded/".length))
    } else {
      CodeType.Other(a)
    }
  }


  val inline = Seq(Embedded.LaTeX, Embedded.HTML)

  val all = Seq(
    SourceCode("javascript"),
    SourceCode("markdown"),
    SourceCode("kotlin"),
    LaTeXMacro,
    Embedded.LaTeX,
    Embedded.HTML,
    Empty,
  )

  val pickler = new Pickler[CodeType] {
    override def pickle(obj: CodeType)(implicit state: PickleState): Unit = {
      state.enc.writeString(obj.str)
    }

    override def unpickle(implicit state: UnpickleState): CodeType = {
      parse(state.dec.readString)
    }
  }

  val jsonFormat = new  Format[CodeType] {
    override def writes(o: CodeType): JsValue = JsString(o.str)

    override def reads(json: JsValue): JsResult[CodeType] = json match {
      case JsString(str) => JsSuccess(parse(str))
      case a => JsError()
    }
  }

}

