package shared


package object data extends ChangeImplicits {

  import boopickle.Default._
  implicit val datePickler = transformPickler((t: Long) => new java.util.Date(t))(_.getTime)

  type ErrorT[T] = Either[ApiError, T]
}
