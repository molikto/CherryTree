package model.data

import api.{ApiError, ClientInit, ErrorT, ServerStatus}
import model._
import model.range.IntRange
import utest._

import scala.util.{Random, Try}

object DataTests extends TestSuite {

  val r = new Random()
  val tests = Tests {
    def testDataObject[T](obj: DataObject[T]): Unit = {
      for (_ <- 0 until 100) {
        val a = obj.random(r)
        val bytes = Pickle.intoBytes(a)(implicitly, obj.pickler)
        val b = Unpickle[T](obj.pickler).fromBytes(bytes)
        assert(a == b)
      }
    }

    'unicode - {
      testDataObject(data.Unicode)
    }

    'rich - {
      testDataObject(data.Rich)
    }

    'richSize - {
      for (_ <- 0 until 100) {
        val a = Rich.random(r)
        assert(a.serialize().size == a.size)
      }
    }

    'richThrowForInvalidData - {
      val org = Rich(Seq(Text.Emphasis(Rich.random(r).text)))
      assert(Try {
        Rich.parse(org.serialize().delete(IntRange(0)))
      }.isFailure)
      assert(Try {
        Rich.parse(org.serialize().delete(IntRange(org.size)))
      }.isFailure)
    }

    'richInfoConsistent - {
      for (i <- 0 until 1000) {
        val p = Rich.random(r)
        (0 until p.size).map(i => p.info(i)) == p.infos
      }
    }

    'content - {
      testDataObject(data.Content)
    }

    'node - {
      testDataObject(data.Node)
    }

    'unicode  - {


      def callSize(a: Unicode): Unicode= {
        val b = a.size
        a
      }
      'sliceSize - {
        assert(Unicode("123456").slice(IntRange(1)).size == Unicode("2").size)
      }
      'replaceSize - {
        assert(Unicode("123456").replace(IntRange(1), Unicode("a")).size == Unicode("1a3456").size)
      }
      'surroundSize - {
        assert(Unicode("123456").surround(IntRange(1), Unicode("a"), Unicode("b")).size == Unicode("1a2b3456").size)
      }


      'sliceSize1 - {
        assert(callSize(Unicode("123456")).slice(IntRange(1)).size == callSize(Unicode("2")).size)
      }
      'replaceSize1 - {
        assert(callSize(Unicode("123456")).replace(IntRange(1), callSize(Unicode("a"))).size == callSize(Unicode("1a3456")).size)
      }
      'surroundSize1 - {
        assert(callSize(Unicode("123456")).surround(IntRange(1), callSize(Unicode("a")), callSize(Unicode("b"))).size == Unicode("1a2b3456").size)
      }

      'slice - {
        assert(Unicode("123456").slice(IntRange(1)) == Unicode("2"))
      }
      'replace - {
        assert(Unicode("123456").replace(IntRange(1), Unicode("a")) == Unicode("1a3456"))
      }
      'surround - {
        assert(Unicode("123456").surround(IntRange(1), Unicode("a"), Unicode("b")) == Unicode("1a2b3456"))
      }
    }

    'implicitlyGenerated - {
      for (i <- 0 until 10) {
        val a: ErrorT[ClientInit] = Right(ClientInit(data.Node.random(r), model.mode.Node.Visual(Seq.empty, Seq.empty), i, ServerStatus(1)))
        val bytes = Pickle.intoBytes(a)
        val b = Unpickle[Either[ApiError, ClientInit]](implicitly).fromBytes(bytes)
        assert(a == b)
      }
    }
  }
}
