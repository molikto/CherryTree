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
      for (i <- 0 until 200) {
        val a = dobj.random(r)
        for (_ <- 0 until 5) {
          val o = obj.random(a, r)
          //        println(s"\nTest No. $i")
          //        println(a)
          //        println(o)
          val res = o(a)
          val kk = res.hashCode()
          val bytes = Pickle.intoBytes(o)(implicitly, obj.pickler)
          val b = Unpickle[O](obj.pickler).fromBytes(bytes)
          if (o.isEmpty) assert(a == res)
          assert(o == b)
          assert(a == o.reverse(a).asInstanceOf[Operation[T, M]].apply(res))

        }
        for (_ <- 0 until 50) {
          val trans = obj.randomTransaction(10, a, r)
          val merged = obj.merge(trans)
          val rest = obj.apply(trans, a)
          val rest2 = obj.apply(merged, a)
          if (rest != rest2) {
            println(trans)
            println(merged)
          }
          assert(rest == rest2)
        }
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
