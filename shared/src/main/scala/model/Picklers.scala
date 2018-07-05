package model

import boopickle._


trait Picklers extends Base with BasicImplicitPicklers with TransformPicklers with TuplePicklers with MaterializePicklerFallback {
  implicit val pickler_Node: Pickler[data.Node] = data.Node.pickler
  implicit val operationPickler_Node: Pickler[operation.Node] = operation.Node.pickler

  implicit val pickler_Unicode: Pickler[data.Unicode] = data.Unicode.pickler
  implicit val operationPickler_Unicode: Pickler[operation.Unicode] = operation.Unicode.pickler
}
