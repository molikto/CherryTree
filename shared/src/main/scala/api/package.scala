import java.nio.ByteBuffer

import boopickle._


package object api {


  import model._
  object PicklerGenerator extends MaterializePicklerFallback {

  }
  implicit val serverStatus = PicklerGenerator.generatePickler[ServerStatus]
  implicit val changeRequest = PicklerGenerator.generatePickler[ChangeRequest]
  implicit val clientInit = PicklerGenerator.generatePickler[ClientInit]
  implicit val clientUpdate = PicklerGenerator.generatePickler[ClientUpdate]


  def pickleState = new PickleState(new EncoderSpeed(), false, false)
  val unpickleState = (bb: ByteBuffer) => new UnpickleState(new DecoderSpeed(bb), false, false)

}
