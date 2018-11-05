package web

import java.text.{SimpleDateFormat}

import command.Key

import scala.scalajs.js
import org.scalajs.dom._

package object ui extends Implicits {

  val EmptyStr = "∅"
  val EvilChar =  "\u200B"



  // https://developer.mozilla.org/zh-CN/docs/Web/API/KeyboardEvent/key/Key_Values
  val KeyMap: Map[String, Key.V] = {
    import Key._
    Map(
      "Home" -> Home,
      "End" -> End,
      "ArrowLeft" -> Left,
      "ArrowRight" -> Right,
      "ArrowUp" -> Up,
      "ArrowDown" -> Down,
      "Enter" -> Enter,
      "PageDown" -> PageDown,
      "PageUp" -> PageUp,
      "Backspace" -> Backspace,
      "Tab" -> Tab,
      "Escape" -> Escape,
      "Shift" -> Shift,
      "Meta" -> Meta,
      "Control" -> Ctrl,
      "Delete" -> Delete,
      "Alt" -> Alt
    )
  }

  val jQ: js.Dynamic = js.Dynamic.global.jQuery

  val CodeMirror: js.Dynamic = js.Dynamic.global.CodeMirror

  val KaTeX = window.asInstanceOf[js.Dynamic].katex


  def jsDate(a: Long) = {
    new js.Date(a.toDouble)
  }

  def formatDate(a: Long) = {
    jsDate(a).asInstanceOf[js.Dynamic].toLocaleString("en-US", jsObject(a => {
      a.hour12 = false
      a.year = "numeric"
      a.month = "short"
      a.day = "numeric"
      a.hour = "2-digit"
      a.minute = "2-digit"
    }))
  }

  def Messages(key: String, args: js.Any*): String = {
    val arr: Seq[js.Any] = Seq(key : js.Any) ++ args
    window.asInstanceOf[js.Dynamic].applyDynamic("Messages")(arr: _*).asInstanceOf[String]
  }

  def svgSourceToBackgroundStr(svg: String): String = {
    val encoded = window.btoa(svg)
    "url(data:image/svg+xml;base64," + encoded + ")"
  }
}
