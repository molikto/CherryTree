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

  def flushBeforeKeyDown(): Unit = {
  }


  def postFlushSelectionOnArrowKey(): Unit = {
  }

  protected var hasShift = false

  def systemHandleArrowKey: Boolean

  override def onAttach(): Unit = {
    super.onAttach()
    event( "keydown", (event: KeyboardEvent) => {
      if (!event.asInstanceOf[js.Dynamic].isComposing.asInstanceOf[Boolean]) {
        flushBeforeKeyDown()
        hasShift = event.keyCode == 16
        var key = KeyMap.get(event.key).orNull
        val isArrow = key == Key.Up || key == Key.Down
        if (systemHandleArrowKey && isArrow) {
          postFlushSelectionOnArrowKey()
        } else {
          if (key == null) {
            key = Key.Grapheme(model.data.Unicode(event.key))
          }
          if (key == null) key = Key.Unknown(event.key)
          val kk = Key(key, meta = event.metaKey, alt = event.altKey, shift = event.shiftKey, control = event.ctrlKey)
          if (!key.isInstanceOf[Key.Modifier]) {
            if (editor.onKeyDown(kk)) preventDefault(event)
          }
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
