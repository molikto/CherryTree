package shared

package object util extends ObservablePropertyImplicits  {


  def maxMin(a: Int, b: Int): (Int, Int) = {
    if (a > b) {
      (a, b)
    } else {
      (b, a)
    }
  }

}
