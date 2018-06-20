//package shared.data0
//
//import utest._
//
//
// LATER make this recover
//object ChangeTests extends TestSuite {
//
//  def assertChange(root: Node, change: Node.Operation, after: Node): Unit = {
//    val changed = ot.node.apply(change, root)
//    assert(after == changed)
////    val reversed = ot.node.apply(changed._1, changed._2)
////    assert(reversed._2 == change)
////    assert(root == reversed._1)
//  }
//
//  val tests = Tests {
//    'node - {
//      'insert - {
//        'simple - {
//          val root = Node.empty(Node.newId())
//          val id = Node.newId()
//          val insert = Change.Node.Insert(Node.Ref.root.withChild(0), Node.empty(id))
//          assertChange(root, insert, root.copy(childs = Seq(Node.empty(id = id))))
//        }
//      }
//      'delete - {
//        'simple - {
//          val id = Node.newId()
//          val root = Node(id, "", Seq(Node("fsdfsa", "Fdsfdsf", Seq.empty)))
//          val delete = Change.Node.Delete(Node.Ref.root.withChild(0))
//          assertChange(root, delete, Node.empty(id))
//        }
//      }
//    }
//    'content - {
//      'insert - {
//        'simple - {
//          val root = Node(Node.newId(), "01234", Seq.empty)
//          val insert = Change.Content.Insert(Node.PointRef(Node.Ref.root, 1), "aa")
//          assertChange(root, insert, root.copy(content = "0aa1234"))
//        }
//      }
//
//      'delete - {
//        'simple - {
//          val root = Node(Node.newId(), "01234", Seq.empty)
//          val insert = Change.Content.Delete(Node.SegmentRef(Node.Ref.root, Node.Content.SegmentRef(1, 3)))
//          assertChange(root, insert, root.copy(content = "04"))
//        }
//      }
//    }
//  }
//}
