package model.data

import boopickle._
import model.data

import scala.util.Random


abstract sealed class Content {
}

/**
  * the reason that content and paragraph is separate is because we
  *
  * HTML, LaTeX and code currently the editing method is all plain text,
  *
  * but paragraph it is not, so we need to be sure that the data we are editing is valid
  */
object Content extends DataObject[Content] {
  case class Code(unicode: Unicode, lang: Option[String]) extends Content
  case class Paragraph(paragraph: data.Paragraph) extends Content {
    val size: Int = paragraph.map(_.size).sum
  }

  override val pickler: Pickler[Content] = new Pickler[Content] {
    override def pickle(obj: Content)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Code(u, l) =>
          writeInt(0)
          Unicode.pickler.pickle(u)
          writeString(l.getOrElse(""))
        case Paragraph(p) =>
          writeInt(1)
          data.Paragraph.pickler.pickle(p)
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
          Paragraph(data.Paragraph.pickler.unpickle)
      }
    }
  }

  override def generateRandom(random: Random): Content = ???
}
