import org.scalajs.dom


package object web {

  def el[T <: dom.raw.HTMLElement] (id: String) = dom.document.getElementById(id).asInstanceOf[T]
}
