package web.view

import web._
import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.raw.{CompositionEvent, Element, Event, HTMLElement, HTMLSpanElement, Node, Range}
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import web.view.doc.DocumentView
import web.view._

import scala.scalajs.js


abstract class SourceEditOption(val str: Unicode, val insert: Boolean, val codeMirrorMode: String) {
  def onDismiss(unicode: Unicode): Unit
}

trait SourceEditOverlay[T <: SourceEditOption] extends OverlayT[T] {

  private val ta = textarea(
    flex := "1 1 auto",
    width := "100%",
    height := "100%",
    name := "code"
  ).render

  dom= form(
    display := "flex",
    flexDirection := "column-reverse",
    padding := "8px",
    `class` := "ct-card unselectable",
    ta).render

  val codeMirror = CodeMirror.fromTextArea(ta, jsObject(a => {
    a.lineNumbers = true
    a.styleActiveLine = true
    a.matchBrackets = true
    a.keyMap = "vim"
    a.lineWrapping = true
    a.showCursorWhenSelecting = true
    a.inputStyle = "contenteditable"
    a.theme = "oceanic-next"
  }))

  codeMirror.getWrapperElement().asInstanceOf[HTMLElement].style.height = "100%"


  {
    val vs = codeMirror.getWrapperElement().asInstanceOf[HTMLElement].getElementsByClassName("CodeMirror-vscrollbar")
    for (i <- 0 until vs.length) {
      vs.item(i).classList.add("ct-scroll")
    }
  }

  var isInnerNormal = true

  CodeMirror.on(codeMirror, "vim-keypress", (e: js.Dynamic) => {
    if (isInnerNormal && e.asInstanceOf[String] == "<Esc>") {
      dismiss()
    }
  })


  CodeMirror.on(codeMirror, "vim-mode-change", (e: js.Dynamic) => {
    val isNormal = e.mode.asInstanceOf[String] == "normal"
    if (!isNormal) isInnerNormal = false
    else window.setTimeout(() => {
      isInnerNormal = true
    }, 0)
  })


  override def focus(): Unit = {
    codeMirror.focus()
  }


  override protected def onDismiss(): Unit = {
    val opt = this.opt
    super.onDismiss()
    opt.onDismiss(model.data.Unicode(codeMirror.getValue().asInstanceOf[String]))
    codeMirror.setValue("")
  }

  override def show(opt: T): Unit = {
    codeMirror.setValue(opt.str.str)
    codeMirror.setOption("mode", opt.codeMirrorMode)
    if (opt.insert) {
      window.asInstanceOf[js.Dynamic].cm = codeMirror
      //codeMirror.enterInsertMode()
    }
    super.show(opt)
  }
}
