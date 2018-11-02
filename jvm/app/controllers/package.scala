import java.nio.ByteBuffer

import akka.util.ByteString
import api.{ChangeRequest, unpickleState}
import boopickle.{PickleState, Pickler, UnpickleState}
import play.api.http.HttpEntity
import model._
import play.api.mvc.{AnyContent, BodyParser, Request}


package object controllers {


  def toEntity[A](a: A)(implicit state: PickleState, pickler: Pickler[A]) = {
    HttpEntity.Strict(ByteString(Pickle.intoBytes(a)), None)
  }

  def fromRequest[A](request: Request[ByteString])
    (implicit buildState: ByteBuffer => UnpickleState, pickler: Pickler[A]) = {
    Unpickle[A](pickler).fromBytes(request.body.toByteBuffer)(buildState)
  }

}
