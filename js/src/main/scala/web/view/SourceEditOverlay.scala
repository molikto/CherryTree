package web.view

import command.Key
import web._
import model._
import model.data.SpecialChar.Delimitation
import model.data._
import model.mode.Content.CodeInside
import model.range.IntRange
import monix.execution.Cancelable
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw._
import org.scalajs.dom.{document, raw, window}
import scalatags.JsDom.all._
import settings.Settings
import web.view.doc.DocumentView

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js


class SourceEditOption(val editor: _root_.view.SourceEditInterface, val str: Unicode, val mode: CodeInside, val codeType: CodeType) {
}


trait SourceEditOverlay[T <: SourceEditOption] extends OverlayT[T] with Settings {


  private var updating = false
  private val dollarSign = Unicode("$")

  def showLineNumber: Boolean = true
  private def exitOnInputDollarSign: Boolean = codeType == Embedded.LaTeX &&
    delimitationSettings.exists(a => a._1 == SpecialChar.LaTeX && a._3 == dollarSign && a._2 == dollarSign)

  private val codeHeight = "calc(100% - 32px)"
  private val ta = textarea(
    height := codeHeight,
    name := "code"
  ).render

  private val desc: HTMLElement = div(
    display := "flex",
    flexDirection := "column",
    justifyContent := "center",
    `class` := "ct-desc").render

  protected def predefined: Seq[SourceEditType]  = SourceEditOverlay.all

  private val selectView: HTMLSelectElement = select(
      height := "24px",
      `class` := "ct-select",
      flex := "0 1 auto",
      alignSelf := "right",
      onchange := { e: Event => {
        if (!updating) {
          val ee = e.target.asInstanceOf[HTMLSelectElement].value
          val ct = predefined.find(_.ct.str == ee).get.ct
          setCodeType(ct)
          opt.editor.onCodeTypeChangeAndEditorUpdated(ct)
        }
      }},
      predefined.map(a => option(a.name, value := a.ct.str))
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
  private var modeChanged = false
  private var modeStr: String = ""
  private var modePos: Int = 0
  private var pendingChanges = new ArrayBuffer[operation.Unicode]()

  def newOp(a: operation.Unicode): Unit = {
    str = a(str)
    pendingChanges.append(a)
  }

  def flush(): Unit = {
    if (!dismissed) {
      if (updating) {
        window.setTimeout(() => {
          flush()
        }, 0)
      } else {
        opt.editor.onChangeAndEditorUpdated(pendingChanges, CodeInside(modeStr, modePos))
        if (pendingChanges.nonEmpty) {
          pendingChanges.clear()
        }
      }
    }
  }

  var keys: String = ""


  private def doUndo() = {
    if (!dismissed) {
      opt.editor.onSourceEditorUndo()
    }
  }

  private def doRedo() = {
    if (!dismissed) {
      opt.editor.onSourceEditorRedo()
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
      val mod = if (model.isMac) "Cmd" else "Ctrl"
      a.extraKeys = CodeMirror.normalizeKeyMap(
        jsObject(a => {
          a.updateDynamic(s"$mod-Z")((a: js.Dynamic) => doUndo())
          a.updateDynamic(s"Shift-${mod}-Z")((a: js.Dynamic) => doRedo())
          a.updateDynamic(s"${mod}-Y")((a: js.Dynamic) => {})
        })
      )
    }))
    CodeMirror.Vim.defineAction("outterRedo", (cm: js.Dynamic, opt: js.Dynamic) => {
      doRedo()
    })
    CodeMirror.Vim.defineAction("outterUndo", (cm: js.Dynamic, opt: js.Dynamic) => {
      doUndo()
    })
    CodeMirror.Vim.mapCommand("u", "action", "outterUndo", jsObject(_ => {}), jsObject(_.context = "normal"))
    CodeMirror.Vim.mapCommand("<C-r>", "action", "outterRedo", jsObject(_ => {}), jsObject(_ => {}))

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
      if (!dismissed) {
        val mm = e.mode.asInstanceOf[String]
        val isNormal = mm == "normal"
        isInnerInsert = mm == "insert"
        if (!isNormal) isInnerNormal = false
        else window.setTimeout(() => {
          if (!dismissed) isInnerNormal = true
        }, 0)
        modeStr = mm
        flush()
      }
    })

    CodeMirror.on(codeMirror, "cursorActivity", (e: js.Dynamic) => {
      if (!dismissed) {
        modePos = codeMirror.indexFromPos(codeMirror.getCursor("head")).asInstanceOf[Int]
      }
    })


    CodeMirror.on(codeMirror, "vim-keypress", (key: js.Dynamic)  => {
      if (!dismissed) {
        if (key.asInstanceOf[String] == "<Esc>") {
          keys = ""
        } else {
          keys = keys + key
        }
        opt.editor.onSourceEditorCommandBuffer(keys)
      }
    })
    CodeMirror.on(codeMirror, "vim-command-done", (e: js.Dynamic) => {
      keys = ""
    })


    CodeMirror.on(codeMirror, "changes", (a: js.Dynamic, change: js.Array[js.Dynamic]) => {
      if (!updating && !dismissed) {
        // LATER change to a line based diff this should be more efficient with code mirror
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
        if (isInnerInsert && from == to && text == "$" && exitOnInputDollarSign && (from == 0 || newVal.substring(from - 1, from) != "\\")) {
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
          flush()
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
    opt.editor.exitCodeEdit()
    codeMirror.setValue("")
    if (isInnerInsert) {
      CodeMirror.Vim.handleKey(codeMirror, "<Esc>", "mapping")
    }
  }

  private var str: Unicode = Unicode.empty

  private var codeType_ : CodeType = null

  def codeType: CodeType = codeType_

  private def setCodeType(a: CodeType) = {
    codeType_ = a
    codeMirror.setOption("mode", a.codeMirror)
    val index = predefined.indexWhere(_.ct == a)
    val ii = if (index >= 0) index else predefined.size - 1
    selectView.selectedIndex = ii

    if (exitOnInputDollarSign) {
      desc.textContent = "insert $ to exit"
    } else {
      desc.textContent = ""
    }
  }

  def sync(a: CodeInside): Unit = {

  }

  def sync(a: CodeType): Unit = {
    updating = true
    setCodeType(a)
    updating = false
  }

  def sync(aa: Seq[operation.Unicode], assertEqual: model.data.Unicode = null): Unit = {
    updating = true
    for (a <- aa) {
      a match {
        case operation.Unicode.Insert(at, u, _) =>
          val strAt =  codeMirror.posFromIndex(str.toStringPosition(at))
          codeMirror.replaceRange(u.str, strAt, strAt)
        case operation.Unicode.Delete(r) =>
          val strS =  codeMirror.posFromIndex(str.toStringPosition(r.start))
          val strE =  codeMirror.posFromIndex(str.toStringPosition(r.until))
          codeMirror.replaceRange("", strS, strE)
        case _ =>
          throw new IllegalArgumentException("Not supported")
      }
      if (aa.size > 1 || assertEqual == null || model.debug_view) {
        str = Unicode(codeMirror.getValue().asInstanceOf[String]).guessProp
      }
    }
    if (assertEqual != null) {
      if (model.debug_view) {
        assert(str == assertEqual, s"not equal $str, $assertEqual")
      }
      str = assertEqual
    }
    updating = false
  }

  override def show(opt: T): Unit = {
    super.show(opt)
    window.asInstanceOf[js.Dynamic].cm = codeMirror
    str = opt.str
    codeType_ = opt.codeType
    updating = true
    codeMirror.setValue(str.str)
    setCodeType(opt.codeType)
    isInnerNormal = true
    isInnerInsert = false
    if (opt.mode.mode == "insert") {
      window.setTimeout(() => {
        if (!dismissed) CodeMirror.Vim.handleKey(codeMirror, "i", "mapping")
      }, 0)
    }
    modeChanged = false
    modeStr = opt.mode.mode
    modePos = opt.mode.pos
    updating = false
  }
}
