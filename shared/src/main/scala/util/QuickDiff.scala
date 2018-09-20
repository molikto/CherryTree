package util

trait QuickDiff[T] {

  protected def idEq(t: T, b: T): Boolean
  protected def eq(a: T, b: T): Boolean = idEq(a, b)

  protected def performChange(index: Int, oldData: T, newData: T)

  protected def performAdd(i: Int, data: T)

  protected def performDelete(i: Int)

  def diff(oldVal: Array[T], newVal: Array[T]) = {
    var start = 0
    var oldEnd = oldVal.length
    var newEnd = newVal.length
    while (start < newEnd && start < oldEnd && {
      val old = oldVal(start)
      val new_ = newVal(start)
      val sameId = idEq(old, new_)
      if (sameId && !eq(old, new_)) {
        performChange(start, old, new_)
      }
      sameId
    }) {
      start += 1
    }
    while (oldEnd > start && newEnd > start && {
      val old = oldVal(oldEnd - 1)
      val new_ = newVal(newEnd - 1)
      val sameId = idEq(old, new_)
      if (sameId && !eq(old, new_)) {
        performChange(oldEnd - 1, old, new_)
      }
      sameId
    }) {
      oldEnd -= 1
      newEnd -= 1
    }
    while (oldEnd != start) {
      performDelete(start)
      oldEnd -= 1
    }
    while (start != newEnd) {
      performAdd(start, newVal(start))
      start += 1
    }
  }

}
