import java.util.UUID

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

package object web {


  def jsObject(a: js.Dynamic => Unit): js.Object = {
    val k =
      js.Object().asInstanceOf[js.Dynamic]
    a(k)
    k.asInstanceOf[js.Object]
  }


  @JSExportTopLevel("uuidOption")
  def uuidOptional(str: String): Option[UUID] = {
    if (str.isEmpty) None else Some(UUID.fromString(str))
  }


  @JSExportTopLevel("uuid")
  def uuid(str: String): UUID = {
    UUID.fromString(str)
  }
}
