package shared

package object data {

  import boopickle.Default._
  implicit val datePickler = transformPickler((t: Long) => new java.util.Date(t))(_.getTime)
}
