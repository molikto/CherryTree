package web.ui.content

import model.data
import model.data.Node.ContentType
import model.data.{Embedded, SourceCode}
import org.scalajs.dom.raw
import org.scalajs.dom.raw.HTMLElement
import view.EditorInterface
import web.view.View
import web.ui.doc.{AbstractDocumentView, DocumentView}


object ContentView {
  type General = ContentView[data.Content, model.operation.Content]
  trait Code extends ContentView[data.Content.Code, model.operation.Content.Code]
  trait Rich extends ContentView[data.Content.Rich, model.operation.Content.Rich]

  def createFromCode(a: data.Content.Code): Code = {
    a.ty match {
      case Embedded("html") =>
        new EmbeddedHtmlView(a)
      case _ =>
        new SourceView(a)
    }
  }


  def findParentContent(t0: raw.Node, dom: HTMLElement, editable: Boolean): ContentView.General = {
    var t = t0
    while (t != null && t != dom) {
      View.maybeDom[View](t) match {
        case Some(a)  =>
          val contentView: ContentView.General = a match {
            case view: RichView =>
              view.asInstanceOf[ContentView.General]
            case view: WrappedCodeView =>
              view.asInstanceOf[ContentView.General]
            case _ =>
              null
          }
          if (contentView != null) {
            return contentView
          }
        case _ =>
      }
      t = t.parentNode
    }
    null
  }

  def matches(a: data.Content, contentType: Option[ContentType], v: General): Boolean = {
    (a, v.asInstanceOf[Any]) match {
      case (data.Content.Rich(r), v: RichView)  => contentType.contains(ContentType.Hr) == v.isHr
      case (c: data.Content.Code, v: Code) => matches(c.ty, v)
      case _ => false
    }
  }

  def matches(a: data.CodeType, v: Code): Boolean = {
    a match {
      case Embedded("html") => v.isInstanceOf[EmbeddedHtmlView]
      case _ => v.isInstanceOf[SourceView]
    }
  }

  def create(a: data.Content, contentType: Option[ContentType], editable: Boolean = false): General = {
    (a match {
      case r: data.Content.Rich =>
        val v = new RichView(r, contentType.contains(ContentType.Hr))
        if (editable) v.dom.style.cursor = "text"
        v
      case s: data.Content.Code => if (editable) new WrappedCodeView(s) else  createFromCode(s)
    }).asInstanceOf[General]
  }
}

trait ContentView[T <: data.Content, O <: model.operation.Content] extends View {

  def createEditor(documentView: AbstractDocumentView, controller: EditorInterface): ContentViewEditor.General

  def tempEditableTempDuringSelectionChange(editable: Boolean): Unit = {}

  private var contentData_ : T = null.asInstanceOf[T]

  final def contentData: T = contentData_

  protected def setInitialContent(a: T) = contentData_ = a

  final def updateContent(c: T, trans: O, viewUpdated: Boolean): Unit = {
    contentData_ = c
    onUpdateContent(c, trans, viewUpdated)
  }

  final def updateContent(c: T): Unit = {
    contentData_ = c
    onUpdateContent(c)
  }

  protected def onUpdateContent(c: T, trans: O, viewUpdated: Boolean): Unit = {
    if (!viewUpdated) {
      onUpdateContent(c)
    }
  }



  protected def onUpdateContent(c: T): Unit = {

  }
}
