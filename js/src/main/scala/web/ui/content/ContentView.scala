package web.ui.content

import model.data
import model.data.{Embedded, SourceCode}
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

  def matches(a: data.Content, v: General): Boolean = {
    a match {
      case data.Content.Rich(r) => v.isInstanceOf[Rich]
      case c: data.Content.Code if v.isInstanceOf[Code] => matches(c.ty, v.asInstanceOf[Code])
      case _ => false
    }
  }

  def matches(a: data.CodeType, v: Code): Boolean = {
    a match {
      case Embedded("html") => v.isInstanceOf[EmbeddedHtmlView]
      case _ => v.isInstanceOf[SourceView]
    }
  }

  def create(a: data.Content, editable: Boolean = false): General = {
    (a match {
      case r: data.Content.Rich =>
        val v = new RichView(r)
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
