package shared.ot







trait SeqSelection[S] {}

object SeqSelection {
  case class Child[S](pos: Int, sel: S) extends SeqSelection[S]
  case class Current[S](from: Int, to: Int) extends SeqSelection[S] {
    assert(from >= 0 && to >= from)
  }
}

class SeqDoc[T, O <: OtOperation[T], C, S](
  override val cot: Doc[T, O, C, S]
) extends SeqOt[T, O, C](cot)
  with Doc[Seq[T], SeqOperation[T, O], SeqConflict[T, C], SeqSelection[S]]{
  override def apply(op: SeqOperation[T, O], sel: SeqSelection[S]): Option[SeqSelection[S]] = {
    op match {
      case SeqOperation.Add(at, cs) =>
        sel match {
          case c@SeqSelection.Child(cpos, csel) =>
            Some(SeqSelection.Child(transformAfterAdded(at, cs.size, cpos), csel))
          case i@SeqSelection.Current(from, to) =>
            Some(SeqSelection.Current(
              transformAfterAdded(at, cs.size, from),
              transformAfterAdded(at, cs.size, to)))
        }
      case SeqOperation.Delete(from, to) =>
        val seg = Segment(from, to)
        sel match {
          case c@SeqSelection.Child(cpos, csel) =>
            transformAfterDeleted(seg, cpos).map(a => SeqSelection.Child(a, csel))
          case i@SeqSelection.Current(f, t) =>
            transformAfterDeleted(seg, Segment(f, t)).map(a => SeqSelection.Current(a.from, a.to))
        }
      case SeqOperation.Child(at, cop) =>
        sel match {
          case SeqSelection.Child(cpos, csel) =>
            cot.apply(cop, csel).map(a => SeqSelection.Child(cpos, a))
          case i@SeqSelection.Current(from, to) =>
            Some(i)
        }
    }
  }
}

