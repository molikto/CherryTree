package shared.ot

import utest._

import shared.api.{ApiError, ClientInit, ErrorT}
import shared.data0._

object PicklerTests extends TestSuite {

  val tests = Tests {
    'listOfListOfString - {
      val ot = OtStringDoc.seqOt.seqOt
      for (i <- 0 until 10) {
        val a = ot.generateRandomData()
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Seq[Seq[String]]].fromBytes(bytes)
        assert(a == b)
      }
    }
    'listOfListOfNode - {
      val ot = Node.Ot
      for (i <- 0 until 10) {
        val a = ot.generateRandomData()
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Node].fromBytes(bytes)
        assert(a == b)
      }
    }

    'implicitlyGenerated - {
      val ot = Node.Ot
      for (i <- 0 until 10) {
        val a: ErrorT[ClientInit] = Right(ClientInit(ot.generateRandomData(), i))
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Either[ApiError, ClientInit]].fromBytes(bytes)
        assert(a == b)
      }
    }
  }
}
