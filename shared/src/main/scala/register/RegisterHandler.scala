package register

import model.data.Unicode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait RegisterInterface {
  def setRegister(a: Int): Unit
  def yank(registerable: Registerable, isDelete: Boolean, register: Int = -1): Unit
  def retrieveSetRegisterAndSetToCloneNode(register: Int = -1): Option[Registerable]
}


trait RegisterHandler extends RegisterInterface {
  private var default: Registerable = null
  private val named = mutable.Map[Int, Registerable]()
  private var zeroToNine = (0 until 10).map(_ => None : Option[Registerable]).toBuffer
  protected var set: Int = -1

  private var system: Registerable = null

  override def setRegister(a: Int): Unit = {
    if ((a >= 'a' && a <= 'z') || (a >= 'A' && a<= 'Z')) {
      set = a
    } else if (a >= '0' && a <= '9') {
      set = a
//    } else if (a == '*') { // this can only be got from Client.set = '*'
//      set = '*'
    } else {
      set = -1
    }
  }

  def registerables: Seq[(Int, Option[Registerable])] =
      Seq(('"'.toInt, Option(default))) ++
        ('a' to 'z').map(i => (i.toInt, named.get(i))) ++
        ('A' to 'Z').map(i => (i.toInt, named.get(i))) ++
      zeroToNine.zipWithIndex.map(a => (a._2 + '0', a._1))


  def currentRegister: Char = if (set == -1) '"' else set.toChar
  protected def getRegisterable(set0: Int = -1): Option[Registerable] = {
    val set = if (set0 == -1) this.set else set0
    if ((set >= 'a' && set <= 'z') || (set >= 'A' && set <= 'Z')) {
      named.get(set)
    } else if (set >= '0' && set <= '9') {
      zeroToNine(set - '0')
    } else if (set == '*') {
      Option(system)
    } else {
      Option(default)
    }
  }
  /**
    * if in current register there is a fresh deleted node, we will mark this as needs clone
    */
  override def retrieveSetRegisterAndSetToCloneNode(register: Int = -1): Option[Registerable] = {
    val set = if (register == -1) {
      val s = this.set
      this.set = -1
      s
    } else {
      register
    }
    val reg = getRegisterable(set)
    reg match {
      case Some(r@Registerable.Node(a, info, needsClone)) if !needsClone =>
        val give = r.copy()
        r.needsClone = true
        r.from = None
        Some(give)
      case a => a
    }

  }

  override def yank(registerable: Registerable, isDelete: Boolean, register: Int = -1): Unit = {
    var set = register
    if (set == -1) {
      set = this.set
      this.set = -1
    }
    var defaultHistory = true
    if (set == -1) {
      default = registerable
    } else if ((set >= 'a' && set <= 'z') || (set >= 'A' && set <= 'Z')) {
      named.put(set, registerable)
    } else if (set >= '0' && set <= '9') {
      defaultHistory = false
      zeroToNine(set - '0') = Some(registerable)
    } else if (set == '*') {
      defaultHistory = false
      system = registerable
    }
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
