package utils.route

import java.util.UUID

import play.api.mvc.{PathBindable, QueryStringBindable}

import scala.util.{Failure, Success, Try}

/**
  * Some route binders.
  */
object Binders {

  /**
    * A `java.util.UUID` bindable.
    */
  implicit object UUIDPathBindable extends PathBindable[UUID] {
    def bind(key: String, value: String) = try {
      Right(UUID.fromString(value))
    } catch {
      case _: Exception => Left("Cannot parse parameter '" + key + "' with value '" + value + "' as UUID")
    }

    def unbind(key: String, value: UUID): String = value.toString
  }

  implicit def queryStringBindable(implicit stringBinder: QueryStringBindable[String]) = new QueryStringBindable[UUID] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, UUID]] = {
      for {
        str <- stringBinder.bind(key, params)
      } yield {
        str match {
          case Right(from) => Try(UUID.fromString(from)) match {
            case Success(a) => Right(a)
            case Failure(t) => Left("Unable to parse UUID")
          }
          case _ => Left("No value")
        }
      }
    }
    override def unbind(key: String, uuid: UUID): String = {
      stringBinder.unbind(key, uuid.toString)
    }
  }
}
