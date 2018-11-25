package model.data

import java.util.UUID

import boopickle.{PickleState, Pickler, UnpickleState}
import jdk.nashorn.internal.runtime.Undefined
import model.range.IntRange
import model.{data, mode}
import play.api.libs.json._
import search.{Search, SearchOccurrence}
import scalatags.Text.all.Frag

import scala.util.Random


abstract sealed class Content {
  def mapBy(map: Map[UUID, UUID]): Content

  def defines(hash: Text.HashTag): Option[IntRange] = None


  def search(a: Search, startPos: Int): Option[IntRange]
  def isEmpty: Boolean
  def size: Int
  def nonEmpty: Boolean = !isEmpty

  def toScalaTags(safe: Boolean): Frag

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
  case class Code(unicode: Unicode, lang: CodeType) extends Content {

    protected override def defaultNormalMode(): mode.Content.Normal = mode.Content.CodeNormal(false)
    protected override def defaultInsertMode(): mode.Content = mode.Content.CodeNormal(true)

    override def isEmpty: Boolean = unicode.isEmpty

    override def size: Int = unicode.size

    override def search(a: Search, startPos: Int): Option[IntRange] = None

    override def mapBy(map: Map[UUID, UUID]): Content = this



    override def toScalaTags(safe: Boolean): Frag = {
      import scalatags.Text.all._
      if (this.lang == Embedded.HTML) {
        if (safe) raw(unicode.str)
        else pre("Embeded HTML")
      }
      else pre(unicode.str)
    }
  }

  object Code {
    val empty: Content = Code(Unicode.empty, CodeType.Empty)
    val jsonFormat: Format[Code] = {
      implicit val codeTypeFormat = CodeType.jsonFormat
      Json.format[Code]
    }
  }

  case class Rich(val content: data.Rich) extends Content {
    override def size: Int = content.size

    protected override def defaultNormalMode(): mode.Content.Normal = mode.Content.RichNormal(content.rangeBeginning)

    protected override def defaultInsertMode(): mode.Content = mode.Content.RichInsert(0)

    override def isEmpty: Boolean = content.isEmpty

    override def search(a: Search, startPos: Int): Option[IntRange] = content.search(a, startPos)

    override def defines(hash: Text.HashTag): Option[IntRange] = content.defines(hash)

    override def mapBy(map: Map[UUID, UUID]): Content = Rich(content.mapBy(map))

    override def toScalaTags(safe: Boolean): Frag = {
      Text.toScalaTags(content.text, safe)
    }
  }

  object Rich {
    val jsonFormat: Format[Rich] = Json.format[Rich]
  }

  val jsonFormat: Format[Content] = new _root_.util.CaseFormat[Content](
    (classOf[Code], "code", Code.jsonFormat),
    (classOf[Rich], "rich", Rich.jsonFormat),
  )


  override val pickler: Pickler[Content] = new Pickler[Content] {
    override def pickle(obj: Content)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case Code(u, l) =>
          writeInt(0)
          Unicode.pickler.pickle(u)
          writeString(l.str)
        case Rich(p) =>
          writeInt(1)
          data.Rich.pickler.pickle(p)
      }
    }

    override def unpickle(implicit state: UnpickleState): Content = {
      import state.dec._
      readInt match {
        case 0 =>
          Code(Unicode.pickler.unpickle, CodeType.parse(readString))
        case 1 =>
          Rich(data.Rich.pickler.unpickle)
      }
    }
  }

  override def random(r: Random): Content =
    if (r.nextBoolean()) {
      Content.Rich(data.Rich.random(r))
    } else {
      Content.Code(Unicode.random(r), CodeType.Empty)
    }
}
