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


class CodeViewEditor(
  val documentView: DocumentView,
  val controller: EditorInterface,
  override val contentView: WrappedCodeView)
  extends ContentViewEditor[model.data.Content.Code, model.operation.Content.Code, model.mode.Content.Code](contentView) {


  private var editing: CoveringSourceEditDialog = null

  def removeEditor(): Unit = {
    if (editing != null) {
      editing.dismiss()
      if (changedInsideEditor) {
        contentView.updateContent()
        changedInsideEditor = false
      }
      editing = null
    }
  }

  override def updateContent(c: model.data.Content.Code, m: Option[model.mode.Content.Code], trans: model.operation.Content.Code, viewUpdated: Boolean, editorUpdated: Boolean): Unit = {
    contentView.contentData = c
    if (editing == null) {
      contentView.updateContent(c, trans, viewUpdated)
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

  import contentView._

  override def updateMode(aa: model.mode.Content.Code, viewUpdated: Boolean, editorUpdated: Boolean, fromUser: Boolean): Unit = {
    dom.classList.add("ct-selection")
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

  contentView.defer(_ => {
    removeEditor()
  })



  override def selectionRect: Rect = {
    toRect(dom.getBoundingClientRect())
  }

}
