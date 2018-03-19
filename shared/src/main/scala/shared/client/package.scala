package shared

import java.nio.ByteBuffer

import autowire.ClientProxy
import boopickle.Default
import shared.api.{Api, ErrorT}

import scala.concurrent.Future


package object client {

  type Server = ClientProxy[Api, ByteBuffer, Default.Pickler, Default.Pickler]


}
