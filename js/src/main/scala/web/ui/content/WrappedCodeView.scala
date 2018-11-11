package web.ui.content

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
import web.ui
import web.ui.doc.{DocumentView, LaTeXMacroCache}
import web.ui._
import web.view._



class WrappedCodeView(
  initData: model.data.Content.Code,
  laTeXMacroCache: LaTeXMacroCache
) extends ContentView.Code {

  override def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General =
    new CodeViewEditor(documentView, controller, this).asInstanceOf[ContentViewEditor.General]
  setInitialContent(initData)

  private var codeView: ContentView.Code = ContentView.createFromCode(initData, laTeXMacroCache)


  override def tempEditableTempDuringSelectionChange(editable: Boolean): Unit = {
    //dom.contentEditable = editable.toString
  }

  protected override def onUpdateContent(contentData: model.data.Content.Code): Unit = {
    if (ContentView.matches(contentData.ty, codeView)) {
      codeView.updateContent(contentData)
    } else {
      codeView.destroy()
      codeView = ContentView.createFromCode(contentData, laTeXMacroCache)
      codeView.attachToNode(dom)
    }
  }


  override def destroy(): Unit = {
    codeView.destroy()
    super.destroy()
  }

  protected override def onUpdateContent(c: model.data.Content.Code, trans: operation.Content.Code, viewUpdated: Boolean): Unit = {
    if (ContentView.matches(c.ty, codeView)) {
      codeView.updateContent(c, trans, viewUpdated)
    } else {
      codeView.destroy()
      codeView = ContentView.createFromCode(c, laTeXMacroCache)
      codeView.attachToNode(dom)
    }
  }

  dom = div(
    position := "relative",
    span(
      position := "absolute",
      ui.EvilChar, // so we can get selection during evil time!
    )
  ).render

  override def onAttach(): Unit = {
    super.onAttach()
    codeView.attachToNode(dom)
  }
}
