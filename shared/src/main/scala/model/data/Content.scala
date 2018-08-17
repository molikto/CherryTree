package model.data

import boopickle._
import model.range.IntRange
import model.{data, mode}

import scala.util.Random


sealed abstract class CodeType(str: String) {
  def source: String = this.asInstanceOf[SourceCode].name
}

case class SourceCode(name: String) extends CodeType(s"source/$name")
case object LaTeXMacro extends CodeType("latex-macro")
case class Embedded(name: String) extends CodeType(s"embedded/$name")
case class Other(name: String) extends CodeType(name)

object CodeType {
  def parse(a: String): CodeType = {
    if (a.startsWith("source/")) {
      SourceCode(a.substring("source/".length))
    } else if (a == "latex-macro") {
      LaTeXMacro
    } else if (a.startsWith("embedded/")) {
      Embedded(a.substring("embedded/".length))
    } else {
      Other(a)
    }
  }
}

abstract sealed class Content {

  def isEmpty: Boolean
  def size: Int
  def nonEmpty: Boolean = !isEmpty

  def defaultNormalMode(): mode.Content.Normal
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

    override def defaultNormalMode(): mode.Content.Normal = mode.Content.CodeNormal

    val ty = CodeType.parse(lang)

    override def isEmpty: Boolean = unicode.isEmpty

    override def size: Int = unicode.size
  }

  object Code {
    val empty: Content = Code(Unicode.empty, "")
  }

  case class Rich(content: data.Rich) extends Content {
    override def size: Int = content.size

    override def defaultNormalMode(): mode.Content.Normal = mode.Content.RichNormal(content.rangeBeginning)

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
