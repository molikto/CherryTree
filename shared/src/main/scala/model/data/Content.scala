package model.data

import boopickle._
import model.range.IntRange
import model.{data, mode}

import scala.util.Random


abstract sealed class Content {
  def defaultMode(): mode.Content = mode.Content.Normal(beginningAtomicRange())
  def beginningAtomicRange(): IntRange
}

/**
  * the reason that content and paragraph is separate is because we
  *
  * HTML, LaTeX and code currently the editing method is all plain text,
  *
  * but rich it is not, so we need to be sure that the data we are editing is valid
  */
object Content extends DataObject[Content] {
  case class Code(unicode: Unicode, lang: Option[String]) extends Content {
    override def beginningAtomicRange(): IntRange = if (unicode.size == 0) IntRange(0, 0) else unicode.extendedGraphemeRange(0)
  }
  case class Rich(content: data.Rich) extends Content {
    val size: Int = content.size
    override def beginningAtomicRange(): IntRange = content.beginningAtomicRange()
  }

  override val pickler: Pickler[Content] = new Pickler[Content] {
    override def pickle(obj: Content)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Code(u, l) =>
          writeInt(0)
          Unicode.pickler.pickle(u)
          writeString(l.getOrElse(""))
        case Rich(p) =>
          writeInt(1)
          data.Rich.pickler.pickle(p)
      }
    }

    override def unpickle(implicit state: UnpickleState): Content = {
      import state.dec._
      readInt match {
        case 0 =>
          Code(Unicode.pickler.unpickle, readString match {
            case "" => None
            case a => Some(a)
          })
        case 1 =>
          Rich(data.Rich.pickler.unpickle)
      }
    }
  }

  override def random(r: Random): Content =
    if (r.nextBoolean()) {
      Content.Rich(data.Rich.random(r))
    } else {
      Content.Code(Unicode.random(r), None)
    }
}
