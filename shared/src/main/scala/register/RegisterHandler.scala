package register

import model.data.Unicode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait RegisterInterface {
  def setRegister(a: Int): Unit
  def yank(registerable: Registerable, isDelete: Boolean): Unit
  def retrieveSetRegisterAndSetToDefault(): Option[Registerable]
}


trait RegisterHandler extends RegisterInterface {
  private var default: Registerable = null
  private val named = mutable.Map[Int, Registerable]()
  private var zeroToNine = (0 until 10).map(_ => None : Option[Registerable]).toBuffer
  private var set: Int = -1

  override def setRegister(a: Int): Unit = {
    if ((a >= 'a' && a <= 'z') || (a >= 'A' && a<= 'Z')) {
      set = a
    } else if (a >= '0' && a <= '9') {
      set = a
    } else {
      set = -1
    }
  }


  override def retrieveSetRegisterAndSetToDefault(): Option[Registerable] = {
    val reg =
      if ((set >= 'a' && set <= 'z') || (set >= 'A' && set <= 'Z')) {
        named.get(set)
      } else if (set >= '0' && set <= '9') {
        zeroToNine(set - '0')
      } else {
        Option(default)
      }
    set = -1
    reg
  }

  override def yank(registerable: Registerable, isDelete: Boolean): Unit = {
    var defaultHistory = true
    if (set == -1) {
      default = registerable
    } else if ((set >= 'a' && set <= 'z') || (set >= 'A' && set <= 'Z')) {
      named.put(set, registerable)
    } else if (set >= '0' && set <= '9') {
      defaultHistory = false
      zeroToNine(set - '0') = Some(registerable)
    }
    set = -1
    if (defaultHistory) {
      if (!isDelete) {
        zeroToNine(0) = Some(registerable)
      } else {
        zeroToNine.insert(1, Some(registerable))
        zeroToNine = zeroToNine.take(10)
      }
    }
  }

}
