package web.ui.doc

import command.Key
import command.Key.{Meta, Up}
import model.data.Unicode
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import view.EditorInterface
import web.view._
import web.ui._

import scala.scalajs.js

object EditorView {

  def extractKey(event: KeyboardEvent): Key = {
    var key = KeyMap.get(event.key).orNull
    if (key == null) {
      key = Key.Grapheme(model.data.Unicode(event.key))
    }
    if (key == null) key = Key.Unknown(event.key)
    Key(key, meta = event.metaKey, alt = event.altKey, shift = event.shiftKey, control = event.ctrlKey)
  }
}
trait EditorView extends View {

  /*
  private def keyboardOf(j: Int, key: String): Unit = {
    for (ev <- Seq("keydown", "keyup")) {

      val keyboardEvent = new KeyboardEvent(ev, web.jsObject(a => {
        a.keyCode = j
        a.key = key
      }).asInstanceOf[KeyboardEventInit])
      keyboardEvent.initKeyboardEvent(
        ev, // event type : keydown, keyup, keypress
        true, // bubbles
        true, // cancelable
        window, // viewArg: should be window
        key, // ctrlKeyArg
        0,
        "",
        false,
        ""
      )
      window.console.log(keyboardEvent)
      document.dispatchEvent(keyboardEvent.asInstanceOf[KeyboardEvent])

//      val e = jQ.Event(ev, init)
//      window.console.log(e)
//      jQ(dom).trigger(e)
    }
  }

  def upEvent() =  {
    keyboardOf(38, "ArrowUp")
  }

  def downEvent() =  {
    keyboardOf(40, "ArrowDown")
  }
  */

  protected def editor: EditorInterface

  protected def flushBeforeKeyDown(): Unit = {
  }


  protected def postFlushSelectionOnSpellCheckerKey(): Unit = {
  }

  protected var hasShift = false

//  def systemHandleArrowKey: Boolean = false

  val triggerReadSelectionKey = Key(";").copy(meta = true)


  override def onAttach(): Unit = {
    super.onAttach()
    event( "keydown", (event: KeyboardEvent) => {
      if (!event.asInstanceOf[js.Dynamic].isComposing.asInstanceOf[Boolean]) {
        flushBeforeKeyDown()
        hasShift = event.keyCode == 16
        val kk = EditorView.extractKey(event)
        val isArrow = (kk.a == Key.Up || kk.a == Key.Down) && !kk.meta && !kk.control
        //        if (systemHandleArrowKey && isArrow) {
        //          postFlushSelectionOnArrowKey()
        //        } else {
        //        }
        if (kk == triggerReadSelectionKey) {
          postFlushSelectionOnSpellCheckerKey()
        } else if (!kk.a.isInstanceOf[Key.Modifier]) {
          if (editor.onKeyDown(kk)) preventDefault(event)
        }
      }
    })

    event( "keyup", (event: KeyboardEvent) => {
      //window.console.log(event)
      hasShift = false
    })

    event( "keypress", (event: KeyboardEvent) => {
      //window.console.log(event)
    })


    /***
      *
      *
      * copy paste currently disabled for entire document
      *
      * // LATER copy paste in normal mode??
      */

    event( "copy", (a: ClipboardEvent) => {
      copyCut(a, false)
      preventDefault(a)
    })

    event( "cut", (a: ClipboardEvent) => {
      copyCut(a, true)
      preventDefault(a)
    })

    def copyCut(a: ClipboardEvent, isCut: Boolean) = {
      val (html, plain, ct) = editor.onExternalCopyCut(isCut)
      a.clipboardData.clearData()
      html.foreach(d => a.clipboardData.setData("text/html", d))
      plain.foreach(d => a.clipboardData.setData("text/plain", d))
      ct.foreach(d => a.clipboardData.setData("text/cherrytree", d))
    }

    event( "paste", (a: ClipboardEvent) => {
      preventDefault(a)
      if (a.clipboardData != null) {
        val html = a.clipboardData.getData("text/html")
        val plain = a.clipboardData.getData("text/plain")
        val ct = a.clipboardData.getData("text/cherrytree")
        if (html != null) {
          window.console.log(html)
        }
        editor.onExternalPasteInRichEditor(Option(html), Option(plain), Option(ct))
      }
    })
  }
}
