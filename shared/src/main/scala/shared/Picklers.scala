package shared

import boopickle._


trait Picklers extends BasicImplicitPicklers with TransformPicklers with TuplePicklers with MaterializePicklerFallback {
  implicit val pickler_Node: Pickler[model.Node] = ot.node.dataPickler
  implicit val operationPickler_Node: Pickler[operation.Node] = ot.node.operationPickler

  implicit val pickler_Unicode: Pickler[model.Unicode] = ot.unicode.dataPickler
  implicit val operationPickler_Unicode: Pickler[operation.Unicode] = ot.unicode.operationPickler
}
