import java.nio.ByteBuffer

import api.{Api, ErrorT}

import scala.concurrent.Future


package object client {


  var localStorage: LocalStorage = new LocalStorage {
    override def set(key: String, str: String): Unit = {
    }

    override def remove(key: String): Unit = {
    }
    override def get(key: String): Option[String] = {
      None
    }
  }
}
