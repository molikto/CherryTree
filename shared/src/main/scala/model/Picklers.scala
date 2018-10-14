package model

import api.ChangeRequest
import boopickle._


trait Picklers extends Base with BasicImplicitPicklers with TransformPicklers with TuplePicklers with MaterializePicklerFallback {


  implicit val pickler_Node: Pickler[data.Node] = data.Node.pickler
  implicit val operationPickler_Node: Pickler[operation.Node] = operation.Node.pickler
  implicit val modePickler_Node: Pickler[mode.Node] = mode.Node.pickler

  implicit val pickler_Content: Pickler[data.Content] = data.Content.pickler
  implicit val operationPickler_Content: Pickler[operation.Content] = operation.Content.pickler

  implicit val pickler_Paragraph: Pickler[data.Rich] = data.Rich.pickler
  implicit val operationPickler_Paragraph: Pickler[operation.Rich] = operation.Rich.pickler

  implicit val pickler_Unicode: Pickler[data.Unicode] = data.Unicode.pickler
  implicit val operationPickler_Unicode: Pickler[operation.Unicode] = operation.Unicode.pickler

  implicit val pickler_EncodedSeq: Pickler[data.EncodedSeq] = data.EncodedSeq.pickler
  implicit val operationPickler_EncodedSeq: Pickler[operation.EncodedSeq] = operation.EncodedSeq.pickler

  implicit val pickler_api_change =  generatePickler[ChangeRequest]

}
