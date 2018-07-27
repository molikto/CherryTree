package web.view.doc

import command.Key
import model.data.Unicode
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import view.EditorInterface
import web.view._

trait EditorView extends View {

  protected def editor: EditorInterface

  def markEditable(dom: HTMLElement): Unit
  def unmarkEditableIfEditable(dom: HTMLElement): Unit
  def unmarkEditable(dom: HTMLElement): Unit


  override def onAttach(): Unit = {
    super.onAttach()
    event( "keydown", (event: KeyboardEvent) => {
      var key = KeyMap.get(event.key).orNull
      if (key == null && Key.isUnicodeKey(event.key)) {
        key = Key.Grapheme(model.data.Unicode(event.key))
      }
      if (key == null) key = Key.Unknown(event.key)
      val kk = Key(key, meta = event.metaKey, alt = event.altKey, shift = event.shiftKey, control = event.ctrlKey)
       // for meta keys, we ignore it, it is mostly browser keys
      // for modifier keys, we also ignore them
      if (!kk.meta && !key.isInstanceOf[Key.Modifier]) {
        if (editor.onKeyDown(kk)) preventDefault(event)
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
      if (a.clipboardData != null) {
        val html = a.clipboardData.getData("text/html")
//        if (html != null) {
//          // LATER copy paste html
//          console.log(html)
//        } else {
//        }
        val plain = a.clipboardData.getData("text/plain")
        if (plain != null) {
          editor.onExternalPastePlain(Unicode(plain))
        }
        // LATER ProseMirror capturePaste...
      }
    })
  }
}
