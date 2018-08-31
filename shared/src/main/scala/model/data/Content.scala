package model.data

import boopickle._
import model.range.IntRange
import model.{data, mode}

import scala.util.Random


case class SourceEditType(name: String, ct: CodeType, desc: String = "")

object SourceEditType {
  val inlineOnly: Seq[SourceEditType] = Seq(
    SourceEditType("Embedded: LaTeX", Embedded.LaTeX),
    SourceEditType("Embedded: HTML", Embedded.HTML)
  )
  val all: Seq[SourceEditType] = (Seq(
    SourceEditType("Source: JavaScript", SourceCode("javascript")),
    SourceEditType("Source: Markdown", SourceCode("markdown")),
    SourceEditType("Source: Kotlin", SourceCode("kotlin")),
    SourceEditType("LaTeX Macro", LaTeXMacro),
    SourceEditType("Plain Text", PlainCodeType)) ++ SourceEditType.inlineOnly).sortBy(_.name) ++
    Seq(SourceEditType("Undefined", EmptyCodeType))
}

sealed abstract class CodeType(val str: String) {
  def codeMirror: String = str
  def delimitation: SpecialChar.Delimitation = if (this == Embedded.LaTeX) {
    SpecialChar.LaTeX
  } else if (this == Embedded.HTML) {
    SpecialChar.HTML
  } else {
    null
  }
}

case class SourceCode(name: String) extends CodeType(s"source/$name") {
  override def codeMirror: String = name
}

case object LaTeXMacro extends CodeType("latex-macro") {
  override def codeMirror: String = "stex"
}
case class Embedded(name: String) extends CodeType(s"embedded/$name") {
  override def codeMirror: String = name
}
object Embedded {
  val LaTeX = Embedded("latex")
  val HTML = Embedded("html")
}
case class OtherCodeType(name: String) extends CodeType(name)
case object PlainCodeType extends CodeType("plain")
case object EmptyCodeType extends CodeType("")

object CodeType {
  def parse(a: String): CodeType = {
    if (a.startsWith("source/")) {
      SourceCode(a.substring("source/".length))
    } else if (a == "latex-macro") {
      LaTeXMacro
    } else if (a == "plain") {
      PlainCodeType
    } else if (a == "") {
      EmptyCodeType
    } else if (a.startsWith("embedded/")) {
      Embedded(a.substring("embedded/".length))
    } else {
      OtherCodeType(a)
    }
  }
}

abstract sealed class Content {

  def isEmpty: Boolean
  def size: Int
  def nonEmpty: Boolean = !isEmpty

  def defaultMode(enableModal: Boolean): mode.Content = if (enableModal) defaultNormalMode() else defaultInsertMode()
  protected def defaultInsertMode(): mode.Content
  protected def defaultNormalMode(): mode.Content.Normal
  def isRich: Boolean = isInstanceOf[Content.Rich]
}

/**
  * the reason that content and paragraph is separate is because we
  *
  * HTML, LaTeX and code currently the editing method is all plain text,
  *
  * but rich it is not, so we need to be sure that the data we are editing is valid
  */
object Content extends DataObject[Content] {

  /**
    *
    * what's in lang
    *
    * code:
    *
    * code/mime
    *
    */
  case class Code(unicode: Unicode, lang: String) extends Content {

    protected override def defaultNormalMode(): mode.Content.Normal = mode.Content.CodeNormal
    protected override def defaultInsertMode(): mode.Content = mode.Content.CodeNormal

    val ty = CodeType.parse(lang)

    override def isEmpty: Boolean = unicode.isEmpty

    override def size: Int = unicode.size

  }

  object Code {
    val empty: Content = Code(Unicode.empty, "")
  }

  case class Rich(content: data.Rich) extends Content {
    override def size: Int = content.size

    protected override def defaultNormalMode(): mode.Content.Normal = mode.Content.RichNormal(content.rangeBeginning)

    protected override def defaultInsertMode(): mode.Content = mode.Content.RichInsert(0)

    override def isEmpty: Boolean = content.isEmpty
  }

  override val pickler: Pickler[Content] = new Pickler[Content] {
    override def pickle(obj: Content)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Code(u, l) =>
          writeInt(0)
          Unicode.pickler.pickle(u)
          writeString(l)
        case Rich(p) =>
          writeInt(1)
          data.Rich.pickler.pickle(p)
      }
    }

    override def unpickle(implicit state: UnpickleState): Content = {
      import state.dec._
      readInt match {
        case 0 =>
          Code(Unicode.pickler.unpickle, readString)
        case 1 =>
          Rich(data.Rich.pickler.unpickle)
      }
    }
  }

  override def random(r: Random): Content =
    if (r.nextBoolean()) {
      Content.Rich(data.Rich.random(r))
    } else {
      Content.Code(Unicode.random(r), "")
    }
}
