package shared.ot



case class DemoProduct(a: Unit, b: Unit)


object DemoProduct {

  type Data = DemoProduct

  val empty: Data = DemoProduct(Unit, Unit)

  sealed trait Operation extends OtOperation {
    val child: OtOperation
    override def isDeletion: Boolean = child.isDeletion
  }
  object Operation {
    case class A(override val child: UnitOperation) extends Operation
    case class B(override val child: UnitOperation) extends Operation
  }

  sealed trait Conflict {
  }

  object Ot extends shared.ot.Ot[Data, Operation, Conflict] {
    override def empty: Data = DemoProduct.empty

    override def apply(c: Operation, data: Data): Data = {
      c match {
        case Operation.A(child) => data.copy(a = UnitOt.apply(child, data.a))
        case Operation.B(child) => data.copy(a = UnitOt.apply(child, data.a))
      }
    }

    override def rebase(winner: Operation, loser: Operation): Rebased[Conflict, Operation] = {
      (winner, loser) match {
        case (Operation.A(wc), Operation.A(lc)) => UnitOt.rebase(wc, lc).map(it => Operation.A(it))
      }
    }

    override val dataSerializer: Serializer[Data] = _
    override val operationSerializer: Serializer[Operation] = _
  }
}

