import client.net.AutowireServer
import org.scalajs.dom


package object client {


  def el[T <: dom.raw.HTMLElement] (id: String) = dom.document.getElementById(id).asInstanceOf[T]

  val api = AutowireServer[shared.Api]
}
