package model.data

import controller.api.{ApiError, ClientInit, ErrorT}
import model._
import model.range.IntRange
import utest._

import scala.util.Try

object DataTests extends TestSuite {

  val tests = Tests {
    def testDataObject[T](obj: DataObject[T]): Unit = {
      for (_ <- 0 until 10) {
        val a = obj.random()
        val bytes = Pickle.intoBytes(a)(implicitly, obj.pickler)
        val b = Unpickle[T](obj.pickler).fromBytes(bytes)
        assert(a == b)
      }
    }
    'node - {
      testDataObject(data.Node)
    }

    'unicode - {
      testDataObject(data.Unicode)
    }

    'content - {
      testDataObject(data.Content)
    }

    'paragraph - {
      testDataObject(data.Paragraph)
    }

    'paragraphSize - {
      for (_ <- 0 until 10) {
        val a = Paragraph.random()
        assert(Paragraph.serialize(a).size == Paragraph.size(a))
      }
    }

    'paragraphThrowForInvalidData - {
      assert(Try {
        Paragraph.parse(Paragraph.serialize(Paragraph(Text.Emphasis(Paragraph.random()))).delete(IntRange(0, SpecialChar.Size)))
      }.isFailure)
    }


    'implicitlyGenerated - {
      for (i <- 0 until 10) {
        val a: ErrorT[ClientInit] = Right(ClientInit(data.Node.random(), i))
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Either[ApiError, ClientInit]](implicitly).fromBytes(bytes)
        assert(a == b)
      }
    }
  }
}
