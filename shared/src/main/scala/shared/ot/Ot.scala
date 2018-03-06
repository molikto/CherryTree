package shared.ot


trait OtOperation[T] {
  def information: Int
}

object Information {
  val Add = 1
  val Delete = 2
  val AddDelete = 3
}


case class Rebased[CONFLICT, T](conflicts: Set[CONFLICT], t: T) {
  def map[G](map: T => G) = Rebased(conflicts, map(t))
}

trait Ot[DATA, OPERATION <: OtOperation[DATA], CONFLICT] {


  lazy val seqOt: Ot[Seq[DATA], SeqOperation[DATA, OPERATION], SeqConflict[DATA, CONFLICT]] =
    new SeqOt(this)

  //def empty: DATA

  def apply(c: Option[OPERATION], data: DATA): DATA = c match {
    case None => data
    case Some(a) => apply(a, data)
  }

  def apply(c: OPERATION, data: DATA): DATA

  def rebase(winner: OPERATION, loser: OPERATION): Rebased[CONFLICT, (Option[OPERATION], Option[OPERATION])]

  val dataSerializer: Serializer[DATA]
  val operationSerializer: Serializer[OPERATION]
}


