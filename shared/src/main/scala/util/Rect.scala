package util

case class Rect(left: Double, top: Double, width: Double, height: Double) extends Comparable[Rect] {

  def distanceMightNeg(xPos: Int): Int = {
    if (contains(xPos, middleY)) {
      - Math.min(Math.abs(left - xPos).toInt, Math.abs(right - xPos).toInt)
    } else {
      Math.min(Math.abs(left - xPos).toInt, Math.abs(right - xPos).toInt)
    }
  }


  def middleX: Double = left + width / 2
  def middleY: Double = top + height / 2

  def contains(a: Double, b: Double): Boolean = (a >= left && a <= right) && (b >= top && b <= bottom)



  def meet(b: Rect): Boolean = {
    def valueInRange(value: Double, min: Double, max: Double)= (value >= min) && (value <= max)
    val A = this
    val B = b
    val xOverlap = valueInRange(A.left, B.left, B.right) ||
      valueInRange(B.left, A.left, A.right)

    val yOverlap = valueInRange(A.top, B.top, B.bottom) ||
      valueInRange(B.top, A.top, A.bottom)
    xOverlap && yOverlap
  }

  def withBorder(h0: Int, v0: Int): Rect = {
    val h = Math.min(width / 2 - 1, h0)
    val v = Math.min(height / 2 - 1, v0)
    Rect(left - h, top - v, Math.max(0, width + h * 2), Math.max(0, height + v * 2))
  }

  //def seemsSameLine(b: Rect): Boolean = meet(b.withBorder(4, -4)) || b.meet(withBorder(4, -4))

  def merge(b: Rect): Rect = Rect.sides(Math.min(left, b.left), Math.min(top, b.top), Math.max(right, b.right), Math.max(bottom, b.bottom))

  def moveBy(l: Double, t: Double) = Rect(left + l, top + t, width, height)

  def bottom: Double =  top + height
  def right: Double = left + width

  override def compareTo(o: Rect): Int = {
    var diff = (left - o.left).signum
    if (diff != 0) return diff
    diff = (top - o.top).signum
    if (diff != 0) return diff
    diff = (width - o.width).signum
    if (diff != 0) return diff
    diff = (height - o.height).signum
    if (diff != 0) return diff
    0
  }
}

object Rect {
  def sides(left: Double, top: Double, right: Double, bottom: Double): Rect = Rect(left, top, right - left, bottom - top)
}
