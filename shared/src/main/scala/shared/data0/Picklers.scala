

         
package shared.data0

import java.nio.ByteBuffer

import shared.ot._

import scala.util._
import boopickle._

trait Picklers extends shared.ot.OtPicklers with Base with BasicImplicitPicklers with TransformPicklers with TuplePicklers with MaterializePicklerFallback {

  implicit val pickler_Document: Pickler[Document] = Document.Ot.dataPickler
  implicit val operationPickler_Document: Pickler[Document.Operation] = Document.Ot.operationPickler
  implicit val pickler_Node: Pickler[Node] = Node.Ot.dataPickler
  implicit val operationPickler_Node: Pickler[Node.Operation] = Node.Ot.operationPickler
}
      