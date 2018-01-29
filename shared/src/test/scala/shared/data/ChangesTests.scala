package shared.data

import utest._
import Utils._


object ChangesTests extends TestSuite {

  val tests = Tests {
    'node - {
      'insert - {
        'simple - {
          val root = Node.empty(Node.newId())
          val id = Node.newId()
          val insert = Change.Node.NewWithParent(Node.Ref.root, id)
          assert(root.copy(childs = Seq(Node.empty(id = id))) == Change.apply(root, insert))
        }
      }
      'delete - {
        'simple - {
          val id = Node.newId()
          val root = Node(id, "", Seq(Node("fsdfsa", "Fdsfdsf", Seq.empty)))
          val delete = Change.Node.Delete(Seq(0))
          assert(Node.empty(id) == Change.apply(root, delete))
        }
      }
    }

  }
}
