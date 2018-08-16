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

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js


abstract class SourceEditOption(val str: Unicode, val insert: Boolean, val codeMirrorMode: String) {
  def onTransaction(unicode: Seq[operation.Unicode]): Unit
  def onDismiss(): Unit
}

trait SourceEditOverlay[T <: SourceEditOption] extends OverlayT[T] {

  def showLineNumber: Boolean = true

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

  private var codeMirror: js.Dynamic = null
  private var pendingChanges = new ArrayBuffer[operation.Unicode]()

  def newOp(a: operation.Unicode): Unit = {
    str = a(str)
    pendingChanges.append(a)
    window.setTimeout(() => flush(), 100)
  }

  def flush(): Unit = {
    if (!dismissed) {
      if (pendingChanges.nonEmpty) {
        opt.onTransaction(pendingChanges)
        pendingChanges.clear()
      }
    }
  }

  private var updating = false

  override def onAttach(): Unit = {
    super.onAttach()
    codeMirror = CodeMirror.fromTextArea(ta, jsObject(a => {
      a.lineNumbers = showLineNumber
      a.styleActiveLine = true
      a.matchBrackets = true
      a.keyMap = "vim"
      a.lineWrapping = true
      a.showCursorWhenSelecting = true
      a.inputStyle = "contenteditable"
      a.theme = "oceanic-next"
    }))

    codeMirror.getWrapperElement().asInstanceOf[HTMLElement].style.height = "100%"


    val vs = codeMirror.getWrapperElement().asInstanceOf[HTMLElement].getElementsByClassName("CodeMirror-vscrollbar")
    for (i <- 0 until vs.length) {
      vs.item(i).classList.add("ct-scroll")
    }

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

    CodeMirror.on(codeMirror, "changes", (a: js.Dynamic, change: js.Array[js.Dynamic]) => {
      if (!updating && !dismissed) {
        val oldVal = str.str
        val newVal = codeMirror.getValue().asInstanceOf[String]
        window.console.log(newVal)
        var start = 0
        var oldEnd = oldVal.length
        var newEnd = newVal.length
        while (start < newEnd && start < oldEnd && oldVal.codePointAt(start) == newVal.codePointAt(start)) {
          start += 1
        }
        while (oldEnd > start && newEnd > start && oldVal.codePointAt(oldEnd - 1) == newVal.codePointAt(newEnd - 1)) {
          oldEnd -= 1
          newEnd -= 1
        }
        val from = start
        val to = oldEnd
        val text = newVal.substring(start, newEnd)
        val fromCp = str.fromStringPosition(from)
        val toCp = str.fromStringPosition(to)
        if (text.isEmpty) {
          newOp(operation.Unicode.Delete(fromCp, toCp))
        } else if (fromCp == toCp) {
          newOp(operation.Unicode.Insert(fromCp, model.data.Unicode(text).guessProp))
        } else {
          newOp(operation.Unicode.Delete(fromCp, toCp))
          newOp(operation.Unicode.Insert(fromCp, model.data.Unicode(text).guessProp))
        }
      }
    })

    window.setTimeout(() => codeMirror.refresh(), 20)
  }

  override def focus(): Unit = {
    codeMirror.focus()
  }

  private var isInnerNormal = true



  override protected def onDismiss(): Unit = {
    val opt = this.opt
    super.onDismiss()
    opt.onDismiss()
    codeMirror.setValue("")
  }

  private var str: Unicode = Unicode.empty

  def sync(a: operation.Unicode): Unit = {
    flush()
    updating = true
    str = a(str)
    codeMirror.setValue(str.str)
    updating = false
  }

  override def show(opt: T): Unit = {
    super.show(opt)
    str = opt.str
    updating = true
    codeMirror.setValue(str.str)
    codeMirror.setOption("mode", opt.codeMirrorMode)
    if (opt.insert) {
      CodeMirror.Vim.handleKey(codeMirror, "i", "mapping")
    }
    updating = false
  }
}
