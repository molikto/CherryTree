package shared.data



sealed class ApiError
object ApiError {


  /**
    * non-fatal error
    */
  case object InvalidToken extends ApiError
  /**
    * non-fatal error due to unreliable transmission channel
    */
  case object ClientVersionIsOlderThanServerCache extends ApiError
}
