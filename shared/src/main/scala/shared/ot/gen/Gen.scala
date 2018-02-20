package shared.ot.gen

import scala.collection.mutable

class Gen {

  sealed trait Ot

  case class ProductOt(var name: String, var childs: Seq[(String, Ot)]) extends Ot
  case class CoproductOt(name: String, cases: Seq[(String, Ot)]) extends Ot
  case object string extends Ot
  case object ot_string extends Ot
  case object int extends Ot
  case class SeqOt(c: Ot) extends Ot
  case class SetOt(c: Ot) extends Ot


  var products = Seq.empty[ProductOt]
  var recursives = Seq.empty[ProductOt]
  var coproducts = Seq.empty[CoproductOt]

  def product(name: String, childs: (String, Ot)*): ProductOt = {
    val a = ProductOt(name, childs.toSeq)
    products = a +: products
    a
  }

  def coproduct(name: String, cases: (String, Ot)*): CoproductOt = {
    val a = CoproductOt(name, cases.toSeq)
    coproducts = a +: coproducts
    a
  }

  def recursive(map: (Ot) => ProductOt) = {
    val p = ProductOt("", Seq.empty)
    val k = map(p)
    p.name = k.name
    p.childs = k.childs
    recursives = p +: recursives
    p
  }

  def seq(a: Ot): Ot = SeqOt(a)
  def set(a: Ot): Ot = SetOt(a)


  def gen(): Unit = {

  }

  def main(args: Array[String]): Unit = {
    gen()
  }
}
