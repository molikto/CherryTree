package shared.ot.gen

import java.io.{BufferedWriter, File, FileWriter}


class Gen(val pkg: String) {


  val dir = new File(pkg.replace('.', '/'))
  dir.mkdirs()

  case class ClassFile(name: String, content: String) {

    def write(): Unit = {
      println(s"Writing $name.scala")
      val file = new File(dir, name + ".scala")
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(
        s"""
           |
         """.stripMargin)
      bw.write(content)
      bw.close()
    }
  }

  sealed trait Ot {
    def isDoc: Boolean
    def ot: String
    def name: String
    def op: String
    def conflict: String
  }

  case class Field(name: String, ty: Ot) {
    val opName = name.take(1).toUpperCase + name.drop(1)
  }

  case class ProductOt(var name0: String, var childs: Seq[Field]) extends Ot {

    override def isDoc: Boolean = childs.forall(_.ty.isDoc)
    override def name: String = name0
    override def ot: String = s"$name.Ot"
    override def op: String = s"$name.Operation"
    override def conflict: String = s"$name.Conflict"

    def gen(): ClassFile = {
 ClassFile(name, s"""
 |package $pkg
 |
 |import shared.ot._
 |
 |
 |case class $name(${childs.map(a => a.name + ": " + a.ty.name).mkString(", ")})
 |
 |
 |object $name {
 |
 |  type Data = $name
 |
 |  sealed trait Operation extends OtOperation[Data] {
 |  }
 |  object Operation {
 |${childs.map(a => s"    case class ${a.opName}(child: ${a.ty.op}) extends Operation { override def information: Int = child.information}").mkString("\n")}
 |  }
 |
 |  sealed trait Conflict {}
 |  object Conflict {
 |${childs.map(a => s"    case class ${a.opName}(child: ${a.ty.conflict}) extends Conflict").mkString("\n")}
 |  }
 |
 |  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {
 |
 |    override def apply(c: Operation, data: Data): Data = {
 |      c match {
 |${childs.map(a => s"        case Operation.${a.opName}(child) => data.copy(${a.name} = ${a.ty.ot}.apply(child, data.${a.name}))").mkString("\n")}
 |      }
 |    }
 |
 |    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, (Option[Operation], Option[Operation])] = {
 |      (winner, loser) match {
 |${childs.map(a => s"        case (Operation.${a.opName}(wc), Operation.${a.opName}(lc)) => val c = ${a.ty.ot}.rebase(wc, lc); Rebased(c.conflicts.map(a => Conflict.${a.opName}(a)), (c.t._1.map(g => Operation.${a.opName}(g)), c.t._2.map(g => Operation.${a.opName}(g))))").mkString("\n")}
 |        case _ => Rebased(Set.empty, (Some(winner), Some(loser)))
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


  case class CoproductOt(name0: String, cases: Seq[(String, Ot)]) extends Ot {
    override def isDoc: Boolean = cases.forall(_._2.isDoc)
    override def name: String = name0
    override def ot: String = s"$name.Ot"
    override def op: String = s"$name.Operation"
    override def conflict: String = s"$name.Conflict"
  }
  case object string extends Ot {
    override def isDoc: Boolean = true
    override def ot: String = "StringDoc"
    override def name: String = "String"
    override def op: String = "StringOperation"
    override def conflict: String = "StringConflict"
  }
  case object ot_string extends Ot {
    override def isDoc: Boolean = true
    override def ot: String = "OtStringDoc"
    override def name: String = "String"
    override def op: String = "OtStringOperation"
    override def conflict: String = "OtStringConflict"
  }
  case object int extends Ot {
    override def isDoc: Boolean = true
    override def ot: String = "IntDoc"
    override def name: String = "Int"
    override def op: String = "IntOperation"
    override def conflict: String = "IntConflict"
  }
  case class SeqOt(c: Ot) extends Ot {
    override def isDoc: Boolean = true
    override def ot: String = s"${c.ot}.seqOt"
    override def name: String = s"Seq[${c.name}]"
    override def op: String = s"SeqOperation[${c.name}, ${c.op}]"
    override def conflict: String = s"SeqConflict[${c.name}, ${c.conflict}]"
  }
  case class SetOt(c: Ot) extends Ot {
    override def isDoc: Boolean = true
    override def ot: String = s"${c.ot}.setOt"
    override def name: String = s"Set[${c.name}]"
    override def op: String = s"SetOperation[${c.op}]"
    override def conflict: String = s"SetConflict[${c.conflict}]"
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
    p.name0 = k.name
    p.childs = k.childs
    recursives = p +: recursives
    p
  }

  def seq(a: Ot): Ot = SeqOt(a)
  def set(a: Ot): Ot = SetOt(a)

  def gen(): Unit = {
    val classes = products.map(_.gen()) ++ recursives.map(_.gen())
    classes.foreach(_.write())
  }

  def main(args: Array[String]): Unit = {
    gen()
  }
}
