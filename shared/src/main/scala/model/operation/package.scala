package model


import model.mode.Mode

import scala.util.Random

package object operation {


  object Type extends Enumeration {
    type Type = Value
    val Structural, Add, Delete, AddDelete = Value

    def reverse(a: Type): Type = a match {
      case Add => Delete
      case Delete => Add
      case a => a
    }
  }
  import Type.Type


  trait Operation[DATA, M <: Mode[DATA]] {
    type This
    def ty: Type
    def apply(data: DATA): DATA

    def transform(a: M): Option[M]
    def transform(a: Option[M]): Option[M] = a.flatMap(transform)
    def reverse(d: DATA): This
    def merge(before: This): Option[This] = None
  }

  trait OperationObject[DATA, M <: Mode[DATA], OPERATION <: Operation[DATA, M]] {

    type TRANSACTION = Seq[OPERATION]

    val pickler: Pickler[OPERATION]

    def random(data: DATA): OPERATION = random(data, new Random())

    def random(data: DATA, r: Random): OPERATION

    def randomTransaction(size: Int, data: DATA, r: Random): TRANSACTION = {
      var a = data
      var i = 0
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
