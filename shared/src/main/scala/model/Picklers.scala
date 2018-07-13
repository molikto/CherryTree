package model

import boopickle._


trait Picklers extends Base with BasicImplicitPicklers with TransformPicklers with TuplePicklers with MaterializePicklerFallback {


  implicit val pickler_Node: Pickler[data.Node] = data.Node.pickler
  implicit val operationPickler_Node: Pickler[operation.Node] = operation.Node.pickler
  implicit val modePickler_Node: Pickler[mode.Node] = mode.Node.pickler

  implicit val pickler_Content: Pickler[data.Content] = data.Content.pickler
  implicit val operationPickler_Content: Pickler[operation.Content] = operation.Content.pickler

  implicit val pickler_Paragraph: Pickler[data.Paragraph] = data.Paragraph.pickler
  implicit val operationPickler_Paragraph: Pickler[operation.Paragraph] = operation.Paragraph.pickler

  implicit val pickler_Unicode: Pickler[data.Unicode] = data.Unicode.pickler
  implicit val operationPickler_Unicode: Pickler[operation.Unicode] = operation.Unicode.pickler
}
