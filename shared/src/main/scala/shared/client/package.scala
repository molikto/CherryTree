package shared

import java.nio.ByteBuffer

import autowire.ClientProxy
import boopickle.Default


package object client {

  type Server = ClientProxy[Api, ByteBuffer, Default.Pickler, Default.Pickler]

}
