import java.nio.ByteBuffer


package object util extends ObservablePropertyImplicits  {

  private val debugOn = false



  def debugged[T](a: T)(implicit debug: Boolean = debugOn): T = {
    if (debug) {
      a match {
        case bb: ByteBuffer =>
          val l = bb.limit()
          val bs = (0 until l).map(_ => bb.get()).toArray
          println(s"[bytebuffer " + bs.mkString(", ") + "]")
          ByteBuffer.wrap(bs).order(bb.order()).asInstanceOf[T]
        case c =>
          println(s"[println $c]")
          c
      }
    } else {
      a
    }
  }

  def maxMin(a: Int, b: Int): (Int, Int) = {
    if (a > b) {
      (a, b)
    } else {
      (b, a)
    }
  }

}
