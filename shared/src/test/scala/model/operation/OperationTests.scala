package model.operation


import api.{ApiError, ClientInit, ErrorT}
import model._
import model.data.DataObject
import model.range.IntRange
import utest._


object OperationTests extends TestSuite {

  val tests = Tests {
    def testOperationObject[T, O <: Operation[T]](dobj: DataObject[T], obj: OperationObject[T, O]): Unit = {
      for (i <- 0 until 1000) {
        val a = dobj.random()
        val o = obj.random(a)
//        println(s"\nTest No. $i")
//        println(a)
//        println(o)
        val res = o(a)
//        println(res)
        val bytes = Pickle.intoBytes(o)(implicitly, obj.pickler)
        val b = Unpickle[O](obj.pickler).fromBytes(bytes)
        assert(o == b)
      }
    }

    'unicode - {
      testOperationObject(data.Unicode, operation.Unicode)
    }

    'paragraph - {
      testOperationObject(data.Paragraph, operation.Paragraph)
    }

    'content - {
      testOperationObject(data.Content, operation.Content)
    }

    'node - {
      testOperationObject(data.Node, operation.Node)
    }
  }
}