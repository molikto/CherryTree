package shared.client

import autowire._
import com.softwaremill.quicklens._
import boopickle.Default._
import shared.api.{Authentication, ClientInit}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class ClientModel(
  protected override val server: Server,
  protected override val initial: ClientInit,
  protected override val authentication: Authentication.Token
) extends ClientModelNodeLevelCommandTrait
