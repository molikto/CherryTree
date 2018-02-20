package shared.ot

trait Serializer[T] {

  def parse(t: Array[Byte]): T

  def serialize(t: T): Array[Byte]

  def seq: Serializer[Seq[T]] = ???
}
