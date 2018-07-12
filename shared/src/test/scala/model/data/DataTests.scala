package model.data

import api.{ApiError, ClientInit, ErrorT}
import model._
import model.range.IntRange
import utest._

import scala.util.Try

object DataTests extends TestSuite {

  val tests = Tests {
    def testDataObject[T](obj: DataObject[T]): Unit = {
      for (_ <- 0 until 100) {
        val a = obj.random()
        val bytes = Pickle.intoBytes(a)(implicitly, obj.pickler)
        val b = Unpickle[T](obj.pickler).fromBytes(bytes)
        assert(a == b)
      }
    }

    'unicode - {
      testDataObject(data.Unicode)
    }

    'paragraph - {
      testDataObject(data.Paragraph)
    }

    'paragraphSize - {
      for (_ <- 0 until 10) {
        val a = Paragraph.random()
        assert(a.serialize().size == a.size)
      }
    }

    'paragraphThrowForInvalidData - {
      val org = Paragraph(Seq(Text.Emphasis(Paragraph.random().text)))
      assert(Try {
        Paragraph.parse(org.serialize().delete(IntRange(0)))
      }.isFailure)
      assert(Try {
        Paragraph.parse(org.serialize().delete(IntRange(org.size)))
      }.isFailure)
    }

    'content - {
      testDataObject(data.Content)
    }

    'node - {
      testDataObject(data.Node)
    }

    'implicitlyGenerated - {
      for (i <- 0 until 10) {
        val a: ErrorT[ClientInit] = Right(ClientInit(data.Node.random(), model.mode.Node.Visual(Seq.empty, Seq.empty), i))
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Either[ApiError, ClientInit]](implicitly).fromBytes(bytes)
        assert(a == b)
      }
    }
  }
}
