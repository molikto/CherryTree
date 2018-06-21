package model

import boopickle._


trait Picklers extends BasicImplicitPicklers with TransformPicklers with TuplePicklers with MaterializePicklerFallback {
  implicit val pickler_Node: Pickler[data.Node] = ot.Node.dataPickler
  implicit val operationPickler_Node: Pickler[operation.Node] = ot.Node.operationPickler

  implicit val pickler_Unicode: Pickler[data.Unicode] = ot.Unicode.dataPickler
  implicit val operationPickler_Unicode: Pickler[operation.Unicode] = ot.Unicode.operationPickler
}
