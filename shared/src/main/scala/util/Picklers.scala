package util

import boopickle.{BasicPicklers, PickleState, Pickler, UnpickleState}
import play.api.libs.json._

trait Picklers {

  val jsonValuePickler: Pickler[JsValue] = new Pickler[JsValue] {
    override def pickle(obj: JsValue)(implicit state: PickleState): Unit = {
      import state.enc._
      obj match {
        case JsString(v) =>
          writeString(v)
        case o@JsObject(u) =>
          writeInt(-1)
          jsonObjectPickler.pickle(o)
        case JsTrue =>
          writeInt(-2)
        case JsFalse =>
          writeInt(-3)
        case JsNumber(value) =>
          writeInt(-4)
          BasicPicklers.BigDecimalPickler.pickle(value)
        case JsNull =>
          writeInt(-5)
        case JsArray(vs) =>
          val kk = - vs.size - 10
          assert(kk < 0)
          writeInt(kk)
          vs.foreach(jsonValuePickler.pickle)
      }
    }

    override def unpickle(implicit state: UnpickleState): JsValue = {
      import state.dec._
      readInt match {
        case -1 => jsonObjectPickler.unpickle
        case -2 => JsTrue
        case -3 => JsFalse
        case -4 => JsNumber(BasicPicklers.BigDecimalPickler.unpickle)
        case -5 => JsNull
        case a if a < 0 =>
          JsArray((0 until (-readInt - 10)).map(_ => jsonValuePickler.unpickle))
        case a => JsString(readString(a))
      }
    }
  }

  val jsonObjectPickler: Pickler[JsObject] = new Pickler[JsObject] {
    override def pickle(obj: JsObject)(implicit state: PickleState): Unit = {
      import state.enc._
      writeInt(obj.value.size)
      obj.value.foreach(pair => {
        writeString(pair._1)
        jsonValuePickler.pickle(pair._2)
      })
    }

    override def unpickle(implicit state: UnpickleState): JsObject = {
      import state.dec._
      JsObject((0 until readInt).map(_ => readString -> jsonValuePickler.unpickle).toMap)
    }
  }
}
