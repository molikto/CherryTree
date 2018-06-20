package shared

/**
  * a cursor represent a insertion point of same kind element in itself
  *
  * when not used as a insertion point, it refers to the element immediately behind it
  */
package object cursor {
  type Unicode = Int
  /**
    * when used as a insertion point, this cannot be empty
    * [0] means insert before the first child, not the node itself
    * when not as a insertion point, [] means the root element
    */
  type Node = Seq[Int]
  type Paragraph = Unicode
}
