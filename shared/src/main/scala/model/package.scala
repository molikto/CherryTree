import boopickle._

package object model extends Picklers {


  def map[T, R](t: (Option[T], Option[T]), f: T => R): (Option[R], Option[R]) = (t._1.map(f), t._2.map(f))

  def some[T, R](a: T, b: R): (Option[T], Option[R]) = (Some(a), Some(b))

}
