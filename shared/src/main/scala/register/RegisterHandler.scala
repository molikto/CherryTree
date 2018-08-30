package register

import model.data.Unicode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait RegisterInterface {
  def setRegister(a: Int): Unit
  def yank(registerable: Registerable, isDelete: Boolean, register: Int = -1): Unit
  def retrieveSetRegisterAndSetToCloneNode(register: Int = -1): Option[Registerable]
  def clearRegister(a: Int): Unit

  def currentRegister: Char
}

object RegisterInterface {
  val ValidRegisters: Seq[Char] =('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Seq('-', '"')
}


trait RegisterHandler extends RegisterInterface {
  private var default: Registerable = null
  private val named = mutable.Map[Int, Registerable]()
  private var zeroToNine = (0 until 10).map(_ => None : Option[Registerable]).toBuffer
  private var smallDelete: Registerable = null
  protected var curRegister: Int = -1

  private var system: Registerable = null

  override def setRegister(a: Int): Unit = {
    if ((a >= 'a' && a <= 'z') || (a >= 'A' && a<= 'Z')) {
      curRegister = a
    } else if (a >= '0' && a <= '9') {
      curRegister = a
      //    } else if (a == '*') { // this can only be got from Client.set = '*'
      //      set = '*'
    } else if (a == '-') {
      curRegister = '-'
    } else {
      curRegister = -1
    }
  }

  def registerables: Seq[(Int, Option[Registerable])] =
      Seq(('"'.toInt, Option(default)), ('-'.toInt, Option(smallDelete))) ++
        ('a' to 'z').map(i => (i.toInt, named.get(i))) ++
        ('A' to 'Z').map(i => (i.toInt, named.get(i))) ++
      zeroToNine.zipWithIndex.map(a => (a._2 + '0', a._1))


  def currentRegister: Char = if (curRegister == -1) '"' else curRegister.toChar

  protected def getRegisterable(set0: Int = -1): Option[Registerable] = {
    val set = if (set0 == -1) this.curRegister else set0
    if ((set >= 'a' && set <= 'z') || (set >= 'A' && set <= 'Z')) {
      named.get(set)
    } else if (set >= '0' && set <= '9') {
      zeroToNine(set - '0')
    } else if (set == '*') {
      Option(system)
    } else if (set == '-') {
      Option(smallDelete)
    } else {
      Option(default)
    }
  }
  /**
    * if in current register there is a fresh deleted node, we will mark this as needs clone
    */
  override def retrieveSetRegisterAndSetToCloneNode(register: Int = -1): Option[Registerable] = {
    val set = if (register == -1) {
      val s = this.curRegister
      this.curRegister = -1
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

  def clearRegister(a: Int) = yank(null, false, a)

  override def yank(registerable: Registerable, isDelete: Boolean, register: Int = -1): Unit = {
    var set = register
    if (set == -1) {
      set = this.curRegister
      this.curRegister = -1
    }
    var defaultHistory = true
    if (set == -1) {
      default = registerable
    } else if ((set >= 'a' && set <= 'z') || (set >= 'A' && set <= 'Z')) {
      defaultHistory = false
      named.put(set, registerable)
    } else if (set >= '0' && set <= '9') {
      defaultHistory = false
      zeroToNine(set - '0') = Option(registerable)
    } else if (set == '"') {
      defaultHistory = false
      default = registerable
    } else if (set == '-') {
      defaultHistory = false
      smallDelete = registerable
    } else if (set == '*') {
      defaultHistory = false
      system = registerable
    }
    if (defaultHistory) {
      if (!isDelete) {
        zeroToNine(0) = Some(registerable)
      } else {
        registerable match {
          case Registerable.Text(a) if a.size == 1 && a.head.isPlain && a.head.asPlain.unicode.size < 10 =>
            smallDelete = registerable
            return
          case _ =>
        }
        zeroToNine.insert(1, Some(registerable))
        zeroToNine = zeroToNine.take(10)
      }
    }
  }

}
