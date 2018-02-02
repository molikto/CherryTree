package shared.client

import autowire._
import com.softwaremill.quicklens._
import shared.data._
import boopickle.Default._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class Client(
  protected override val server: Server,
  protected override val initial: ClientState
) extends ClientStateTrait
