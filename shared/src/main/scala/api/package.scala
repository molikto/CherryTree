import java.nio.ByteBuffer

import boopickle.{PickleState, UnpickleState, _}


package object api {


  import model._
  object PicklerGenerator extends MaterializePicklerFallback {

  }



  implicit val collabratorPickler = PicklerGenerator.generatePickler[Collaborator]
  implicit val serverStatus = PicklerGenerator.generatePickler[ServerStatus]
  implicit val initRequst = PicklerGenerator.generatePickler[InitRequest]
  implicit val clientInit = PicklerGenerator.generatePickler[InitResponse]
  implicit val changeRequest = PicklerGenerator.generatePickler[ChangeRequest]
  implicit val clientUpdate = PicklerGenerator.generatePickler[ChangeResponse]
  implicit val listResultPickler = PicklerGenerator.generatePickler[ListResult]
  implicit val nodeInfoPickler = PicklerGenerator.generatePickler[NodeInfo]


  implicit def pickleState: PickleState = new PickleState(new EncoderSize, false, false)

  implicit def unpickleState: ByteBuffer => UnpickleState =
    bytes => new UnpickleState(new DecoderSize(bytes), false, false)

}
