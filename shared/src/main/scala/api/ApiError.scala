package api

sealed abstract class ApiError extends Exception
object ApiError {


  case object ClientVersionIsHigherThanServerCache extends ApiError

  case object ClientVersionTooOld extends ApiError
}
