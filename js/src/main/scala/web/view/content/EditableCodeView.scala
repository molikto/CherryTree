package web.view.content

import model._
import model.data.{Content, _}
import model.mode.Content.CodeInside
import model.operation.Content
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import util.Rect
import view.EditorInterface
import web.view.doc.DocumentView
import web.view.{EmptyStr, removeAllChild, theme}
import web.view._

import scala.scalajs.js


class EditableCodeView(
  val documentView: DocumentView,
  val controller: EditorInterface,
  override var contentData: model.data.Content.Code)
  extends EditableContentView[model.data.Content.Code, model.operation.Content.Code, model.mode.Content.Code] {

  var codeView: ContentView.Code = ContentView.createFromCode(contentData)

  dom = div(
  ).render

  codeView.attachToNode(dom)

  private var editing: CoveringSourceEditDialog = null

  def removeEditor(): Unit = {
    if (editing != null) {
      editing.dismiss()
      if (changedInsideEditor) {
        updateContent()
        changedInsideEditor = false
      }
      editing = null
    }
  }

  override def updateContent(c: model.data.Content.Code, m: Option[model.mode.Content.Code], trans: model.operation.Content.Code, viewUpdated: Boolean, editorUpdated: Boolean): Unit = {
    this.contentData = c
    codeView.contentData = c
    if (editing == null) {
      if (ContentView.matches(c.ty, codeView)) {
        codeView.updateContent(c, trans, viewUpdated)
      } else {
        codeView.destroy()
        codeView = ContentView.createFromCode(c)
        codeView.attachToNode(dom)
      }
    } else {
      changedInsideEditor = true
      if (!editorUpdated) {
        trans match {
          case model.operation.Content.CodeLang(lang) =>
            editing.sync(c.ty)
          case model.operation.Content.CodeContent(op) =>
            editing.sync(Seq(op), c.unicode)
        }
      }
    }
  }


  var changedInsideEditor = false

  // this will only be called when editor is null
  override def updateContent(): Unit = {
    if (ContentView.matches(contentData.ty, codeView)) {
      codeView.updateContent()
    } else {
      codeView.destroy()
      codeView = ContentView.createFromCode(contentData)
      codeView.attachToNode(dom)
    }
  }

  override def updateMode(aa: model.mode.Content.Code, viewUpdated: Boolean, editorUpdated: Boolean, fromUser: Boolean): Unit = {
    if (fromUser) {
      web.view.scrollInToViewIfNotVisible(dom, documentView.dom)
    }
    aa match {
      case model.mode.Content.CodeNormal =>
        removeEditor()
      case inside: model.mode.Content.CodeInside =>
        if (editing == null) {
          editing = documentView.sourceEditor
          editing.show(new SourceEditOption(controller, contentData.unicode, CodeInside.empty, contentData.ty))
        } else if (!editorUpdated) {
          editing.sync(inside)
        }
    }
  }

  override def clearMode(): Unit = {
    removeEditor()
    dom.classList.remove("ct-selection")
  }


  /**
    * will also remove from parent
    * ALSO make sure you destroy child dom attachments!!!
    */
  override def destroy(): Unit = {
    clearMode()
    codeView.destroy()
    super.destroy()
  }


  override def focus(): Unit = {
    codeView.focus()
  }

  override def initMode(): Unit = {
    dom.classList.add("ct-selection")
  }

  override def selectionRect: Rect = {
    toRect(dom.getBoundingClientRect())
  }

}
