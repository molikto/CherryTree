package util

import boopickle.{PickleState, UnpickleState}
import play.api.libs.json._

import scala.reflect.ClassTag

class CaseFormat[T](vals: (Class[_], String, Format[_])*) extends Format[T] {

  override def reads(json: JsValue): JsResult[T] =
    json match {
      case o: JsObject =>
        val keys = o.keys
        if (keys.size == 1) {
          val key = keys.head
          vals.find(_._2 == key) match {
            case Some(k) =>
              k._3.reads(o(key)).asInstanceOf[JsResult[T]]
            case None =>
              JsError()
          }
        } else {
          JsError()
        }
      case _ => JsError()
    }

  override def writes(o: T): JsValue = {
    vals.find(_._1 == o.getClass).get._3.asInstanceOf[Format[T]].writes(o)
  }
}

trait Formats {
}
