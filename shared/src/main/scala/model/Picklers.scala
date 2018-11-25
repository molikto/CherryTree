package model

import java.nio.ByteBuffer

import api.ChangeRequest
import boopickle._


/**
  * picklers for data objects,
  */
trait Picklers extends Base with BasicImplicitPicklers with TransformPicklers with TuplePicklers with util.Picklers {


  implicit def pickler_Node: Pickler[data.Node] = data.Node.pickler
  implicit def operationPickler_Node: Pickler[operation.Node] = operation.Node.pickler
  implicit def modePickler_Node: Pickler[mode.Node] = mode.Node.pickler

  implicit def pickler_Content: Pickler[data.Content] = data.Content.pickler
  implicit def operationPickler_Content: Pickler[operation.Content] = operation.Content.pickler

  implicit def pickler_Paragraph: Pickler[data.Rich] = data.Rich.pickler
  implicit def operationPickler_Paragraph: Pickler[operation.Rich] = operation.Rich.pickler

  implicit def pickler_Unicode: Pickler[data.Unicode] = data.Unicode.pickler
  implicit def operationPickler_Unicode: Pickler[operation.Unicode] = operation.Unicode.pickler

  implicit def pickler_EncodedSeq: Pickler[data.EncodedSeq] = data.EncodedSeq.pickler
  implicit def operationPickler_EncodedSeq: Pickler[operation.EncodedSeq] = operation.EncodedSeq.pickler
  implicit def pickler_codeType: Pickler[data.CodeType] = data.CodeType.pickler

}
