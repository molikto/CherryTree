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


  def pickleState = new PickleState(new EncoderSpeed(), false, false)
  val unpickleState = (bb: ByteBuffer) => new UnpickleState(new DecoderSpeed(bb), false, false)

}
