package model

import boopickle.Pickler
import model.data.DataObject

import scala.util.Random

package object operation {


  object Type extends Enumeration {
    type Type = Value
    val Structural, Add, Delete, AddDelete = Value
  }
  import Type.Type


  trait Operation[DATA] {
    def ty: Type
    def apply(data: DATA): DATA
  }

  trait OperationObject[DATA, OPERATION <: Operation[DATA]] {

    type TRANSACTION = Seq[OPERATION]

    val pickler: Pickler[OPERATION]

    def random(data: DATA): OPERATION = random(data, new Random())

    def random(data: DATA, random: Random): OPERATION

    def randomTransaction(size: Int, data: DATA): TRANSACTION = {
      var a = data
      var i = 0
      val r = new Random()
      var cs = Seq.empty[OPERATION]
      while (i < size) {
        val c = random(a, r)
        a = c.apply(a)
        cs = cs :+ c
        i += 1
      }
      cs
    }

    def apply(c: Option[OPERATION], model: DATA): DATA = c match {
      case None => model
      case Some(a) => a.apply(model)
    }

    def apply(cs: TRANSACTION, model: DATA): DATA = {
      cs.foldLeft(model) { (model, c) => c.apply(model) }
    }

    def applyT(cs: Seq[TRANSACTION], model: DATA): DATA = {
      cs.foldLeft(model) { (model, c) => apply(c, model) }
    }
  }
}
