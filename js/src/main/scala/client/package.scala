import java.nio.ByteBuffer

import autowire.ClientProxy
import boopickle.Default
import client.net.AutowireServer
import org.scalajs.dom
import shared.Api


package object client {

  type Server = ClientProxy[Api, ByteBuffer, Default.Pickler, Default.Pickler]

  def el[T <: dom.raw.HTMLElement] (id: String) = dom.document.getElementById(id).asInstanceOf[T]
}
