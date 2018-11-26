import java.nio.ByteBuffer

import boopickle._
import monix.reactive.Observable
import register.Registerable

import scala.concurrent.Future

package object model extends Picklers with Formats {


  def map[T, R](t: (Seq[T], Seq[T]), f: T => R): (Seq[R], Seq[R]) = (t._1.map(f), t._2.map(f))

  def some[T, R](a: T, b: R): (Seq[T], Seq[R]) = (Seq(a), Seq(b))


  var debug_testing = false
  val debug_katex = false
  var debug_view = false
  val debug_scroll = debug_view && false
  var debug_model = false
  var debug_webSocket = true
  val debug_transmit = false
  val debug_selection = debug_view && false
  val debug_play = true

  var debug_RenderEmptyInsertionPointAsBox = false


}
