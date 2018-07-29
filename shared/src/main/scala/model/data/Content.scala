package model.data

import boopickle._
import model.range.IntRange
import model.{data, mode}

import scala.util.Random


abstract sealed class Content {
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

    def isSource: Boolean = lang.startsWith("source/")
    def asSourceMime: String = {
      if (isSource)
        lang.substring("source/".length)
      else
        ""
    }
  }
  case class Rich(content: data.Rich) extends Content {
    def size: Int = content.size

    override def defaultNormalMode(): mode.Content.Normal = mode.Content.RichNormal(content.rangeBeginning)
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
