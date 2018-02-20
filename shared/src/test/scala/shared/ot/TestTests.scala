package shared.ot

import utest._


object TestTests extends TestSuite {

  val tests = Tests {
    'nothing - {
      val a = new SeqOt(UnitOt)
    }
  }
}
