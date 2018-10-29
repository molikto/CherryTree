package util

import boopickle.{PickleState, UnpickleState}

trait Picklers {



  def writeStringMap(map: Map[String, String])(implicit a: PickleState) = {
    import a.enc._
    writeInt(map.size)
    map.foreach(pair => {
      writeString(pair._1)
      writeString(pair._2)
    })
  }

  def readStringMap(implicit a: UnpickleState): Map[String, String] = {
    import a.dec._
    (0 until readInt).map(_ => readString -> readString).toMap
  }
}
