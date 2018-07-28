package model.operation


import api.{ApiError, ClientInit, ErrorT}
import model._
import model.data.DataObject
import model.mode.Mode
import model.range.IntRange
import utest._

import scala.util.Random


object OperationTests extends TestSuite {


  val r = new Random()
  val tests = Tests {
    def testOperationObject[T, M <: Mode[T], O <: Operation[T, M]](dobj: DataObject[T], obj: OperationObject[T, M, O]): Unit = {
      for (i <- 0 until 1000) {
        val a = dobj.random(r)
        val o = obj.random(a, r)
//        println(s"\nTest No. $i")
//        println(a)
//        println(o)
        val res = o(a)
        val kk = res.hashCode()
        val bytes = Pickle.intoBytes(o)(implicitly, obj.pickler)
        val b = Unpickle[O](obj.pickler).fromBytes(bytes)
        assert(o == b)
        assert(a == o.reverse(a).asInstanceOf[Operation[T, M]].apply(res))
      }
    }

    'unicode - {
      testOperationObject(data.Unicode, operation.Unicode)
    }

    'rich - {
      testOperationObject(data.Rich, operation.Rich)
    }

    'content - {
      testOperationObject(data.Content, operation.Content)
    }

    'node - {
      testOperationObject(data.Node, operation.Node)
    }
  }
}
