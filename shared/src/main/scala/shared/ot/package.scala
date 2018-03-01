package shared

package object ot {


  object StringOt extends AtomicOt[String]
  type StringOperation = AtomicOt.Operation[String]
  type StringConflict = AtomicOt.Conflict[String]

  object IntOt extends AtomicOt[Int]
  type IntOperation = AtomicOt.Operation[Int]
  type IntConflict = AtomicOt.Conflict[Int]
}
