package shared.data

import utest._


object RebaseTests extends TestSuite {


  val tests = Tests {

    val randomText = "fdlksf lkasdfjklsa dfklds fjdsklf djsklf dslkfj slfjksla fjdsalfshgdsljfk"
    val node: Node = Node.testFromText(
      s"""1. $randomText
         |  1.1 $randomText
         |    1.1.1 $randomText
         |    1.1.2 $randomText
         |    1.1.3 $randomText
         |  1.2 $randomText
         |  1.3 $randomText
         |    1.3.1 $randomText
         |  1.4 $randomText""".stripMargin)
    def assertRebase(a: Change, b: Change): Unit = {
      val bp = a.rebase(b)
      val ap = b.rebase(a)
      assert(Change.apply(Change.apply(node, a)._1, bp) == Change.apply(Change.apply(node, b)._1, ap))
    }

    'simple - {
    }
  }
}
