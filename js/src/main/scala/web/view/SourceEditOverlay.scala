package web.view

import command.Key
import web._
import model._
import model.data._
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw._
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import web.view.doc.DocumentView
import web.view._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js


abstract class SourceEditOption(val str: Unicode, val insert: Boolean, val codeType: CodeType) {
  def onCodeTypeChange(to: CodeType): Unit
  def onTransaction(unicode: Seq[operation.Unicode]): Unit
  def onDismiss(): Unit
}
object SourceEditOverlay {
  val inlineOnly: Seq[(String, CodeType)] = Vector(
    ("Embedded: LaTeX", LaTeXEmbedded)
  )
  val all = (Vector(
    ("Source: JavaScript", SourceCode("javascript")),
    ("Source: Markdown", SourceCode("markdown")),
    ("Embedded: HTML", Embedded("html")),
    ("LaTeX Macro", LaTeXMacro),
    ("Plain Text", PlainCodeType)) ++ SourceEditOverlay.inlineOnly).sortBy(_._1) ++
    Vector(("Undefined", EmptyCodeType))
}

trait SourceEditOverlay[T <: SourceEditOption] extends OverlayT[T] {


  private var updating = false

  def showLineNumber: Boolean = true
  def exitOnInputDollarSign: Boolean = false

  private val codeHeight = "calc(100% - 32px)"
  private val ta = textarea(
    height := codeHeight,
    name := "code"
  ).render

  protected def desc: HTMLElement = div().render

  protected def predefined: Seq[(String, CodeType)]  = SourceEditOverlay.all

  private val selectView: HTMLSelectElement = select(
      height := "24px",
      `class` := "ct-select",
      flex := "0 1 auto",
      alignSelf := "right",
      onchange := { e: Event => {
        if (!updating) {
          val ee = e.target.asInstanceOf[HTMLSelectElement].value
          val ct = predefined.find(_._2.str == ee).get._2
          opt.onCodeTypeChange(ct)
          setCodeType(ct)
        }
      }},
      predefined.map(a => option(a._1, value := a._2.str))
    ).render

  dom = form(
    display := "flex",
    flexDirection := "column",
    padding := "8px",
    `class` := "ct-card unselectable",
    ta,
    div(height := "24px",
      marginTop := "8px",
      alignContent := "middle",
      display := "flex",
      justifyContent := "space-between",
      flexDirection := "row",
      desc,
      selectView
    )).render

  selectView.addEventListener("keydown", (k: KeyboardEvent) => {
    val kk = KeyMap.get(k.key)
    if (kk.contains(Key.Tab) && !k.shiftKey) {
      k.preventDefault()
      focus()
//    } else if (kk.contains(Key.Escape) && !k.shiftKey) {
//      k.preventDefault()
//      dismiss()
    }
  })

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

    codeMirror.getWrapperElement().asInstanceOf[HTMLElement].style.height = codeHeight


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
      val mm = e.mode.asInstanceOf[String]
      val isNormal = mm == "normal"
      isInnerInsert = mm == "insert"
      if (!isNormal) isInnerNormal = false
      else window.setTimeout(() => {
        isInnerNormal = true
      }, 0)
    })

    CodeMirror.on(codeMirror, "changes", (a: js.Dynamic, change: js.Array[js.Dynamic]) => {
      if (!updating && !dismissed) {
        val oldVal = str.str
        val newVal = codeMirror.getValue().asInstanceOf[String]
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
        if (isInnerInsert && exitOnInputDollarSign && from == to && text == "$" && (from == 0 || newVal.substring(from - 1, from) != "\\")) {
          dismiss()
        } else {
          if (text.isEmpty) {
            newOp(operation.Unicode.Delete(fromCp, toCp))
          } else if (fromCp == toCp) {
            newOp(operation.Unicode.Insert(fromCp, model.data.Unicode(text).guessProp))
          } else {
            newOp(operation.Unicode.Delete(fromCp, toCp))
            newOp(operation.Unicode.Insert(fromCp, model.data.Unicode(text).guessProp))
          }
        }
      }
    })

    window.setTimeout(() => codeMirror.refresh(), 20)
  }

  override def focus(): Unit = {
    codeMirror.focus()
  }

  private var isInnerNormal = true
  private var isInnerInsert = false



  override protected def onDismiss(): Unit = {
    val opt = this.opt
    super.onDismiss()
    opt.onDismiss()
    codeMirror.setValue("")
  }

  private var str: Unicode = Unicode.empty

  private def setCodeType(a: CodeType) = {
    codeMirror.setOption("mode", a.codeMirror)
    val index = predefined.indexWhere(_._2 == a)
    val ii = if (index >= 0) index else predefined.size - 1
    selectView.selectedIndex = ii
  }

  def sync(a: CodeType): Unit = {
    flush()
    updating = true
    setCodeType(a)
    updating = false
  }

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
    setCodeType(opt.codeType)
    if (opt.insert) {
      CodeMirror.Vim.handleKey(codeMirror, "i", "mapping")
    }
    updating = false
  }
}
