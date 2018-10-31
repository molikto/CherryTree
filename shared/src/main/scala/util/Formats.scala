package util

import boopickle.{PickleState, Pickler, UnpickleState}

trait Picklers {


  implicit val stringMapPickler: Pickler[Map[String, String]] = new Pickler[Map[String, String]] {
    override def pickle(obj: Map[String, String])(implicit state: PickleState): Unit = {
      writeStringMap(obj)
    }

    override def unpickle(implicit state: UnpickleState): Map[String, String] = {
      readStringMap
    }
  }

  def toArray[T](a: T)(implicit state: PickleState, pickler: Pickler[T]) = {
    pickler.pickle(a)
    state.toByteBuffer.array()
  }

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
