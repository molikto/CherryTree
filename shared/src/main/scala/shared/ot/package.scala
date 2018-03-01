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
}
