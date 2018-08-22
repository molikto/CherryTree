package web.view.doc

import command.Key
import command.Key.Up
import model.data.Unicode
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import view.EditorInterface
import web.view._

import scala.scalajs.js

object EditorView {

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

  def readSelectionOnKeyUpDown(): Unit

  override def onAttach(): Unit = {
    super.onAttach()
    event( "keydown", (event: KeyboardEvent) => {
      var key = KeyMap.get(event.key).orNull
      if (key == Key.Down || key == Key.Up) {
        readSelectionOnKeyUpDown()
      } else {
        // TODO better handling this
        var isBiy = false
        var isZo = false
        if (key == null && Key.isUnicodeKey(event.key)) {
          key = Key.Grapheme(model.data.Unicode(event.key))
          isBiy = "biy".contains(event.key)
          isZo = "zo".contains(event.key)
        }
        if (key == null) key = Key.Unknown(event.key)
        val kk = Key(key, meta = event.metaKey, alt = event.altKey, shift = event.shiftKey, control = event.ctrlKey)
        // for meta keys, we ignore it, it is mostly browser keys
        // for modifier keys, we also ignore them
        val allow = if (!kk.meta) {
          true
        } else {
          if (isBiy) {
            !kk.shift
          } else if (isZo) {
            true
          } else {
            false
          }
        }
        if (allow && !key.isInstanceOf[Key.Modifier]) {
          if (editor.onKeyDown(kk)) preventDefault(event)
        }
      }
    })

    event( "keyup", (event: KeyboardEvent) => {
      //window.console.log(event)
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
      window.console.log(a)
      preventDefault(a)
    })

    event( "cut", (a: ClipboardEvent) => {
      window.console.log(a)
      preventDefault(a)
    })

    event( "paste", (a: ClipboardEvent) => {
      preventDefault(a)
      window.console.log(a.clipboardData)
      if (a.clipboardData != null) {
        val html = a.clipboardData.getData("text/html")
//        if (html != null) {
//          // LATER copy paste html
//          console.log(html)
//        } else {
//        }
        val plain = a.clipboardData.getData("text/plain")
        if (plain != null) {
          editor.onExternalPasteInRichEditor(Unicode(plain))
        }
        // LATER ProseMirror capturePaste...
      }
    })
  }
}
