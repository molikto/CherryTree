package client

import java.nio.ByteBuffer

import autowire._
import com.softwaremill.quicklens._
import api.{Api, Authentication, ClientInit}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object Client {
  type Proxy = ClientProxy[Api, ByteBuffer, boopickle.Pickler, boopickle.Pickler]
}
class Client(
  protected override val server: Client.Proxy,
  protected override val initial: ClientInit,
  protected override val authentication: Authentication.Token
) extends ClientStateTrait
