package util

case class Rect(left: Double, top: Double, width: Double, height: Double) {


  def contains(a: Double, b: Double): Boolean = (a >= left && a <= right) && (b >= top && b <= bottom)

  def meet(b: Rect): Boolean =
    b.contains(left, top) ||
      b.contains(left, bottom) ||
      b.contains(right, top) ||
      b.contains(right, bottom) ||
      contains(b.left, b.top) ||
      contains(b.left, b.bottom) ||
      contains(b.right, b.top) ||
      contains(b.right, b.bottom)


  def withBorder(h: Int, v: Int): Rect = Rect(left - h, top - v, Math.max(0, width + h * 2), Math.max(0, height + v * 2))

  def seemsSameLine(b: Rect): Boolean = meet(b.withBorder(4, 0))

  def merge(b: Rect): Rect = Rect.sides(Math.min(left, b.left), Math.min(top, b.top), Math.max(right, b.right), Math.max(bottom, b.bottom))

  def moveBy(l: Double, t: Double) = Rect(left + l, top + t, width, height)

  def bottom: Double =  top + height
  def right: Double = left + width
}

object Rect {
  def sides(left: Double, top: Double, right: Double, bottom: Double): Rect = Rect(left, top, right - left, bottom - top)
}
