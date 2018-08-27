package web.ui

import org.scalajs.dom.html.Span
import scalatags.JsDom
import scalatags.JsDom.all._

package object content {



  def warningInline(str: String): JsDom.TypedTag[Span] = {
    span(display := "inline-block", span(`class` := "ct-warning-inline", str))
  }

  def errorInline(str: String, th: Throwable = null): JsDom.TypedTag[Span] = {
    span(display := "inline-block", span(`class` := "ct-error-inline", str))
  }
}
