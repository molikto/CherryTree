package model

import java.nio.ByteBuffer

import api.ChangeRequest
import boopickle._
import play.api.libs.json.Format


/**
  * picklers for data objects,
  */
trait Picklers extends Base with BasicImplicitPicklers with TransformPicklers with TuplePicklers {


  implicit val pickler_Node: Format[data.Node] = data.Node.jsonFormat
  implicit val operationPickler_Node: Format[operation.Node] = operation.Node.jsonFormat
  implicit val modePickler_Node: Format[mode.Node] = mode.Node.jsonFormat

  implicit val pickler_Content: Format[data.Content] = data.Content.jsonFormat
  implicit val operationPickler_Content: Format[operation.Content] = operation.Content.jsonFormat

  implicit val pickler_Paragraph: Format[data.Rich] = data.Rich.jsonFormat
  implicit val operationPickler_Paragraph: Format[operation.Rich] = operation.Rich.jsonFormat

  implicit val pickler_Unicode: Format[data.Unicode] = data.Unicode.jsonFormat
  implicit val operationPickler_Unicode: Format[operation.Unicode] = operation.Unicode.jsonFormat

  implicit val pickler_EncodedSeq: Format[data.EncodedSeq] = data.EncodedSeq.jsonFormat
  implicit val operationPickler_EncodedSeq: Format[operation.EncodedSeq] = operation.EncodedSeq.jsonFormat
}
