package shared.ot

object AtomicDoc {
  sealed trait Selection
  object Selection {
    case object Just extends Selection
  }
}
abstract class AtomicDoc[DATA] extends AtomicOt[DATA] with Doc[DATA, AtomicOt.Operation[DATA], AtomicOt.Conflict[DATA], AtomicDoc.Selection] {
  override def apply(op: AtomicOt.Operation[DATA], sel: AtomicDoc.Selection): Option[AtomicDoc.Selection] = Some(sel)
}
