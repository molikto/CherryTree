package shared



package object ot {


  object StringOt extends AtomicOt[String] {
    override val dataSerializer: Serializer[String] = ???
    override val operationSerializer: Serializer[AtomicOt.Operation[String]] = ???
  }
  type StringOperation = AtomicOt.Operation[String]
  type StringConflict = AtomicOt.Conflict[String]

  object IntOt extends AtomicOt[Int] {
    override val dataSerializer: Serializer[Int] = ???
    override val operationSerializer: Serializer[AtomicOt.Operation[Int]] = ???
  }
  type IntOperation = AtomicOt.Operation[Int]
  type IntConflict = AtomicOt.Conflict[Int]


  case class Segment(from: Int, to: Int) {
    def contains(p: Int) = p >= from && p <= to
    def size = to - from + 1
  }


  def transformAfterAdded(point: Int, size: Int, p: Int): Int = {
    if (p < point) {
      p
    } else {
      p + size
    }
  }

  def transformAfterDeleted(s: Segment, p: Int): Option[Int] = {
    if (p < s.from) {
      Some(p)
    } else if (s.contains(p)) {
      None
    } else {
      Some(p - s.size)
    }
  }

  /**
    * @return None if either side of `s` is deleted
    */
  def transformAfterDeleted(d: Segment, f: Segment): Option[Segment] = {
    val l = transformAfterDeleted(d, f.from)
    val r = transformAfterDeleted(d, f.to)
    (l, r) match {
      case (Some(ll), Some(rr)) => Some(Segment(ll, rr))
      case _ => None
    }
  }

  def transformDeletingSegmentAfterDeleted(d: Segment, f: Segment): Option[Segment] = {
    val l = transformAfterDeleted(d, f.from)
    val r = transformAfterDeleted(d, f.to)
    (l, r) match {
      case (Some(ll), Some(rr)) => Some(Segment(ll, rr))
      case (Some(ll), None) => Some(Segment(ll, d.from - 1))
      case (None, Some(rr)) => Some(Segment(d.from, rr))
      case (None, None) =>  None
    }
  }
}
