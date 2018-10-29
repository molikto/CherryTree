import java.nio.ByteBuffer

import boopickle._


package object api {


  import model._
  object PicklerGenerator extends MaterializePicklerFallback {

  }


  implicit val serverStatus = PicklerGenerator.generatePickler[ServerStatus]
  implicit val initRequst = PicklerGenerator.generatePickler[InitRequest]
  implicit val clientInit = PicklerGenerator.generatePickler[InitResponse]
  implicit val changeRequest = PicklerGenerator.generatePickler[ChangeRequest]
  implicit val clientUpdate = PicklerGenerator.generatePickler[ChangeResponse]


  implicit def pickleState: model.PickleState = new PickleState(new EncoderSpeed(), false, false)
  implicit val unpickleState: ByteBuffer => model.UnpickleState = (bb: ByteBuffer) => new UnpickleState(new DecoderSpeed(bb), false, false)

}
