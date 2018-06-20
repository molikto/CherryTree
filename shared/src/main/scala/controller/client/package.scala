package controller

import java.nio.ByteBuffer

import autowire.ClientProxy
import api.{Api, ErrorT}

import scala.concurrent.Future


package object client {

  type Server = ClientProxy[Api, ByteBuffer, boopickle.Pickler, boopickle.Pickler]
}
