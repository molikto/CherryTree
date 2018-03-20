

         
package shared.data0

import shared.ot._
import scala.util._
import boopickle._

trait Picklers extends shared.ot.OtPicklers {
  implicit val pickler_Document: Pickler[Document] = Document.Ot.dataPickler
  implicit val operationPickler_Document: Pickler[Document.Operation] = Document.Ot.operationPickler
  implicit val pickler_Node: Pickler[Node] = Node.Ot.dataPickler
  implicit val operationPickler_Node: Pickler[Node.Operation] = Node.Ot.operationPickler
}
      