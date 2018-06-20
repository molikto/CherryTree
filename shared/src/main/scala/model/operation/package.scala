package model

package object operation {


  object Type extends Enumeration {
    type Type = Value
    val Structural, Add, Delete, AddDelete = Value
  }
  import Type.Type


  trait Operation[MODEL] {
    def ty: Type
    def apply(data: MODEL): MODEL
  }

}
