package controller.client

import autowire._
import com.softwaremill.quicklens._

import controller.api.{Authentication, ClientInit}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class Client(
  protected override val server: Server,
  protected override val initial: ClientInit,
  protected override val authentication: Authentication.Token
) extends ClientStateTrait
