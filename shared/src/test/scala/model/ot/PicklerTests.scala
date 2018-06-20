package model.ot

import utest._

import controller.api.{ApiError, ClientInit, ErrorT}
import model._
import boopickle._

object PicklerTests extends TestSuite {

  val tests = Tests {
    'listOfListOfString - {
      val o = ot.unicode
      for (i <- 0 until 10) {
        val a = o.generateRandomModel()
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Seq[Seq[String]]].fromBytes(bytes)
        assert(a == b)
      }
    }
    'listOfListOfNode - {
      val o = ot.node
      for (i <- 0 until 10) {
        val a = o.generateRandomModel()
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Node].fromBytes(bytes)
        assert(a == b)
      }
    }

    'implicitlyGenerated - {
      val o = ot.node
      for (i <- 0 until 10) {
        val a: ErrorT[ClientInit] = Right(ClientInit(o.generateRandomModel(), i))
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Either[ApiError, ClientInit]].fromBytes(bytes)
        assert(a == b)
      }
    }
  }
}
