package controller.api

sealed abstract class ApiError extends Exception
object ApiError {


  /**
    * non-fatal error
    */
  case object InvalidToken extends ApiError
  /**
    * non-fatal error due to unreliable transmission channel
    */
  case object ClientVersionIsOlderThanServerCache extends ApiError

  case object ClientVersionIsHigherThanServerCache extends ApiError
}
