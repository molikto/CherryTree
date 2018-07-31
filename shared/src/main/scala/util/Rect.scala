package util

case class Rect(left: Double, top: Double, width: Double, height: Double) {
  def moveBy(l: Double, t: Double) = Rect(left - l, top - t, width, height)

  def bottom: Double =  top + height
  def right: Double = left + width
}
