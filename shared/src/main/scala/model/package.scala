import boopickle._
import client.LocalStorage
import register.Registerable

package object model extends Picklers {


  def map[T, R](t: (Seq[T], Seq[T]), f: T => R): (Seq[R], Seq[R]) = (t._1.map(f), t._2.map(f))

  def some[T, R](a: T, b: R): (Seq[T], Seq[R]) = (Seq(a), Seq(b))

  var parseFromCommonMarkMarkdown: String => data.Node = null
  var parseFromHtml: String => Registerable = null

  var oldDocVersion = false
  val debug_view = false
  val debug_scroll = debug_view && true
  val debug_model = false
  val debug_selection = debug_view && false

  var debug_RenderEmptyInsertionPointAsBox = false

  var isMac: Boolean = false

}
