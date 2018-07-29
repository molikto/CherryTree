import boopickle._

package object model extends Picklers {


  def map[T, R](t: (Seq[T], Seq[T]), f: T => R): (Seq[R], Seq[R]) = (t._1.map(f), t._2.map(f))

  def some[T, R](a: T, b: R): (Seq[T], Seq[R]) = (Seq(a), Seq(b))


  var oldDocVersion = false
  var debugView = true
  var debugModel = false

  var debugRenderEmptyInsertionPointAsBox = false
  var debugDisableFocusHandling = false

  var isMac: Boolean = false
}
