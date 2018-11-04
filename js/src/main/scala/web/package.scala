import scala.scalajs.js

package object web {


  val debug_fakeSelection = false

  def jsObject(a: js.Dynamic => Unit): js.Object = {
    val k =
      js.Object().asInstanceOf[js.Dynamic]
    a(k)
    k.asInstanceOf[js.Object]
  }

}
