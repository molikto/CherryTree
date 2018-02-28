package shared.ot.gen


class Gen(val pkg: String) {

  case class ClassFile(name: String, content: String)

  sealed trait Ot {
    def ot: String
    def name: String
    def op: String
  }

  case class Field(name: String, ty: Ot) {
    val opName = name.take(1).toUpperCase + name.drop(1)
  }

  case class ProductOt(override var name: String, var childs: Seq[Field]) extends Ot {

    override def ot: String = s"$name.Ot"
    override def op: String = s"$name.Operation"

    def gen(): ClassFile = {
 ClassFile(name, s"""
 |package $pkg
 |
 |
 |
 |case class $name(${childs.map(a => a.name + ": " + a.ty.name).mkString(", ")})
 |
 |
 |object $name {
 |
 |  type Data = $name
 |
 |  sealed trait Operation extends OtOperation {
 |    val child: OtOperation
 |    override def isDeletion: Boolean = child.isDeletion
 |  }
 |  object Operation {
 |${childs.map(a => s"    case class ${a.opName}(override val child: ${a.ty.op}) extends Operation\n")}
 |  }
 |
 |  sealed trait Conflict {
 |  }
 |
 |  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {
 |
 |    override def apply(c: Operation, data: Data): Data = {
 |      c match {
 |${childs.map(a => s"        case Operation.${a.opName}(child) => data.copy(${a.name} = ${a.ty.ot}.apply(child, data.${a.name}))\n")}
 |      }
 |    }
 |
 |    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, (Operation, Operation)] = {
 |      (winner, loser) match {
 |${childs.map(a => s"        case (Operation.${a.opName}(wc), Operation.${a.opName}(lc)) => ${a.ty.ot}.rebase(wc, lc).map(it => Operation.${a.opName}(it))\n")}
          case _ => Rebased((winner, loser))
 |      }
 |    }
 |
 |    override val dataSerializer: Serializer[Data] = _
 |    override val operationSerializer: Serializer[Operation] = _
 |  }
 |}
       """.stripMargin)
    }

  }


  case class CoproductOt(override val name: String, cases: Seq[(String, Ot)]) extends Ot {
    override def ot: String = s"$name.Ot"
    override def op: String = s"$name.Operation"
  }
  case object string extends Ot {
    override def ot: String = "StringOt"
    override def name: String = "String"
    override def op: String = "StringOperation"
  }
  case object ot_string extends Ot {
    override def ot: String = "OtStringOt"
    override def name: String = "String"
    override def op: String = "OtStringOperation"
  }
  case object int extends Ot {
    override def ot: String = "IntOt"
    override def name: String = "Int"
    override def op: String = "IntOperation"
  }
  case class SeqOt(c: Ot) extends Ot {
    override def ot: String = s"${c.ot}.seqOt"
    override def name: String = s"Seq[${c.name}]"
    override def op: String = s"SeqOperation[${c.op}]"
  }
  case class SetOt(c: Ot) extends Ot {
    override def ot: String = s"${c.ot}.setOt"
    override def name: String = s"Set[${c.name}]"
    override def op: String = s"SetOperation[${c.op}]"
  }


  var products = Seq.empty[ProductOt]
  var recursives = Seq.empty[ProductOt]
  var coproducts = Seq.empty[CoproductOt]

  def product(name: String, childs: (String, Ot)*): ProductOt = {
    val a = ProductOt(name, childs.toSeq.map(a => Field(a._1, a._2)))
    products = a +: products
    a
  }

  def coproduct(name: String, cases: (String, Ot)*): CoproductOt = {
    val a = CoproductOt(name, cases.toSeq)
    coproducts = a +: coproducts
    a
  }

  def recursive(map: (Ot) => ProductOt): ProductOt = {
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
    products.map(_.gen()) ++ recursives.map(_.gen())
  }

  def main(args: Array[String]): Unit = {
    gen()
  }
}
