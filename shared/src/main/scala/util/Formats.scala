package util

import play.api.libs.json._


object Formats {

}
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
    val stuff = vals.find(_._1 == o.getClass).get
    JsObject(Seq(stuff._2 -> stuff._3.asInstanceOf[Format[T]].writes(o)))
  }
}

