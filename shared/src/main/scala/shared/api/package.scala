package shared

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

package object api {

  import boopickle.Default._
  implicit val datePickler = transformPickler((t: Long) => new java.util.Date(t))(_.getTime)

  type ErrorT[T] = Either[ApiError, T]

  def transform[T](future: Future[ErrorT[T]]): Future[T] = future.transform {
    case Success(a) =>
      a match {
        case Left(err) =>
          Failure(err)
        case Right(c) =>
          Success(c)
      }
    case Failure(e) =>
      Failure(e)
  }
}
