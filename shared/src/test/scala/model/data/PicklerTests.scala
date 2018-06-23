package model.data

import controller.api.{ApiError, ClientInit, ErrorT}
import model._
import utest._

object PicklerTests extends TestSuite {

  val tests = Tests {
    'listOfListOfString - {
      for (i <- 0 until 10) {
        val a = operation.Unicode.random()
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Seq[Seq[String]]].fromBytes(bytes)
        assert(a == b)
      }
    }
    'listOfListOfNode - {
      val o = ot.Node
      for (i <- 0 until 10) {
        val a = operation.Unicode.random()
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[data.Node].fromBytes(bytes)
        assert(a == b)
      }
    }

    'implicitlyGenerated - {
      val o = ot.Node
      for (i <- 0 until 10) {
        val a: ErrorT[ClientInit] = Right(ClientInit(operation.Node.random(), i))
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Either[ApiError, ClientInit]].fromBytes(bytes)
        assert(a == b)
      }
    }
  }
}
