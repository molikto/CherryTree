package shared.data

import shared.data.Node.Ref
import utest._


object ChangesTests extends TestSuite {

  def assertChange(root: Node, change: Change, after: Node): Unit = {
    val changed = Change.apply(root, change)
    assert(after == changed._1)
    val reversed = Change.apply(changed._1, changed._2)
    assert(reversed._2 == change)
    assert(root == reversed._1)
  }

  val tests = Tests {
    'node - {
      'insert - {
        'simple - {
          val root = Node.empty(Node.newId())
          val id = Node.newId()
          val insert = Change.Node.Insert(Node.Ref.root.withChild(0), Node.empty(id))
          assertChange(root, insert, root.copy(childs = Seq(Node.empty(id = id))))
        }
      }
      'delete - {
        'simple - {
          val id = Node.newId()
          val root = Node(id, "", Seq(Node("fsdfsa", "Fdsfdsf", Seq.empty)))
          val delete = Change.Node.Delete(Node.Ref.root.withChild(0))
          assertChange(root, delete, Node.empty(id))
        }
      }
    }

  }
}
