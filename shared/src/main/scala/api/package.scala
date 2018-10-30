import java.nio.ByteBuffer

import play.api.libs.json.Json



package object api {


  import model._


  implicit val serverStatus = Json.format[ServerStatus]
  implicit val initRequst = Json.format[InitRequest]
  implicit val clientInit = Json.format[InitResponse]
  implicit val changeRequest = Json.format[ChangeRequest]
  implicit val clientUpdate = Json.format[ChangeResponse]

}
