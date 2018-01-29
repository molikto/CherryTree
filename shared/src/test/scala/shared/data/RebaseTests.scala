package shared.data

import utest._


object RebaseTests extends TestSuite {


  val tests = Tests {

    val randomText = "fdlksf lkasdfjklsa dfklds fjdsklf djsklf dslkfj slfjksla fjdsalfshgdsljfk"
    val node: Node = Node.testFromText(
      s""" $randomText
         |  0 $randomText
         |    0.0 $randomText
         |    0.1 $randomText
         |    0.2 $randomText
         |    0.3 $randomText
         |  1 $randomText
         |  2 $randomText
         |    2.0 $randomText
         |    2.1 $randomText
         |  3 $randomText
         |  4 $randomText
         |  """.stripMargin)
    def assertRebase(a: Change, b: Change): Unit = {
      println(s"Change a: $a")
      println(s"Change b: $b")
      (a.rebaseOption(b), b.rebaseOption(a)) match {
        case (Some(bp), Some(ap)) =>
          println(s"Change a': $ap")
          println(s"Change b': $bp")
          val app0 = Change.apply(Change.apply(node, a)._1, bp)._1
          val app1 = Change.apply(Change.apply(node, b)._1, ap)._1
          if (app0 == app1) {
            println(s"App: $app0")
          } else {
            println(s"App 0: $app0")
            println(s"App 1: $app1")
          }
          assert(app0 == app1)
        case (None, None) => Unit
          // match error here
      }
    }
    val insert0 =
      Change.Node.Insert(Node.Ref.root.withChild(0), Node(Node.newId(), "insert0", Seq.empty))
    val insert02 =
      Change.Node.Insert(Node.Ref.root.withChilds(0,2), Node(Node.newId(), "insert02", Seq.empty))
    val delete0 = Change.Node.Delete(Node.Ref.root.withChild(0))
    val delete00 = Change.Node.Delete(Node.Ref.root.withChilds(0,0))
    val delete01 = Change.Node.Delete(Node.Ref.root.withChilds(0,1))
    val delete02 = Change.Node.Delete(Node.Ref.root.withChilds(0,2))
    val delete20 = Change.Node.Delete(Node.Ref.root.withChilds(0,0))
    val insert0t = Change.Content.Insert(Node.PointRef(Node.Ref.root, 4), Node.Content.testRandom())
    val insert03t = Change.Content.Insert(Node.PointRef(Node.Ref.root.withChilds(0,3), 20), Node.Content.testRandom())

    val changes = Seq(insert0, insert02, delete0, delete00, delete01, delete02, delete20, insert0t, insert03t)

    'simple - {
      for (c <- changes) {
        for (cc <- changes) {
          assertRebase(c, cc)
        }
      }
    }
  }
}
