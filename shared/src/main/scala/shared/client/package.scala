package shared

import java.nio.ByteBuffer

import autowire.ClientProxy
import shared.api.{Api, ErrorT}

import scala.concurrent.Future


package object client {

  type Server = ClientProxy[Api, ByteBuffer, boopickle.Pickler, boopickle.Pickler]


}
