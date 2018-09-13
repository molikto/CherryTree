package register

import model.data.{Text, Unicode}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait RegisterInterface {
  def setRegister(a: Int): Unit
  def yank(registerable: Registerable, isDelete: Boolean, register: Int = -1): Unit
  def retrieveSetRegisterAndSetToCloneNode(allowNode: Boolean, register: Int = -1): Option[Registerable]
  def clearRegister(a: Int): Unit

  def currentRegister: Char
}

object RegisterInterface {
  val ValidRegisters: Seq[Char] =('a' to 'z') ++ ('0' to '9') ++ Seq('-', '"')
}



trait RegisterHandler extends RegisterInterface {
  private var default: Registerable = null
  private val named = mutable.Map[Int, Registerable]()
  private var zeroToNine = (0 until 10).map(_ => None : Option[Registerable]).toBuffer
  private var smallDelete: Registerable = null
  protected var curRegister: Int = -1
  protected var append = false

  private var system: Registerable = null

  protected var registerJustSet = false

  override def setRegister(a: Int): Unit = {
    registerJustSet = true
    append = false
    if (a >= 'a' && a <= 'z') {
      curRegister = a
    } else if (a >= 'A' && a <= 'Z') {
      append = true
      curRegister = a - ('A' - 'a')
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
      zeroToNine.zipWithIndex.map(a => (a._2 + '0', a._1))


  def currentRegister: Char = if (curRegister == -1) '"' else curRegister.toChar

  protected def getRegisterable(set0: Int = -1): Option[Registerable] = {
    val set = if (set0 == -1) this.curRegister else set0
    if (set >= 'a' && set <= 'z') {
      named.get(set)
    } else if (set >= 'A' && set <= 'Z') {
      named.get(set - 'A' + 'a')
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
  override def retrieveSetRegisterAndSetToCloneNode(allowNode: Boolean, register: Int = -1): Option[Registerable] = {
    val set = if (register == -1) {
      val s = this.curRegister
      this.curRegister = -1
      s
    } else {
      register
    }
    val reg = getRegisterable(set)
    val res = reg match {
      case Some(r@Registerable.Node(a, deletionFrom)) =>
        if (allowNode) {
          if (deletionFrom.isEmpty) {
            reg
          } else {
            val noNeedClone = r.copy()
            r.deletionFrom = None
            Some(noNeedClone)
          }
        } else {
          None
        }
      case a => a
    }
    res
  }

  def clearRegister(a: Int) = yank(null, false, a)

  override def yank(registerable0: Registerable, isDelete: Boolean, register: Int = -1): Unit = {
    var registerable = registerable0
    if (registerable.isEmpty) return
    var set = register
    if (set == -1) {
      set = this.curRegister
      if (append) {
        set = set + 'A' - 'a'
        append = false
      }
      this.curRegister = -1
    }
    var push: Boolean = false
    var defaultHistory = true
    if (set == -1) {
      default = registerable
    } else if (set >= 'a' && set <= 'z') {
      defaultHistory = false
      named.put(set, registerable)
    } else if (set >= 'A' && set <= 'Z') {
      defaultHistory = false
      val low = set - 'A' + 'a'
      registerable = named.get(low) match {
        case Some(t) =>
          (t, registerable) match {
            case (Registerable.Text(a), Registerable.Text(b)) =>
              Registerable.Text(Text.normalize(a ++ b))
            case (Registerable.Text(a), Registerable.Unicode(b)) =>
              Registerable.Text(a :+ Text.Code(b))
            case (Registerable.Unicode(a), Registerable.Text(b)) =>
              Registerable.Text(Text.Code(a) +: b)
            case (Registerable.Unicode(a), Registerable.Unicode(b)) =>
              Registerable.Unicode(a + b)
            case _ =>
              registerable
          }
        case None =>
          registerable
      }
      named.put(low, registerable)
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
    default = registerable
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
    registerable match {
      case Registerable.Node(na, deletionFrom) if deletionFrom.nonEmpty =>
        registerables.flatMap(_._2).foreach(a => {
          if (a != registerable) {
            a match {
              case b@Registerable.Node(nb, df) if df.nonEmpty =>
                if (uuids(na).exists(uuids(nb))) {
                  b.deletionFrom = None
                }
              case _ =>
            }
          }
        })
      case _ =>
    }
  }

  def markAllAsNeedsClone(): Unit = {
    registerables.flatMap(_._2).foreach {
      case b@Registerable.Node(nb, nc)  =>
        b.deletionFrom = None
      case _ =>
    }
  }

  private def uuids(u: Seq[model.data.Node]): mutable.Set[String] = {
    val c = mutable.Set.empty[String]
    u.foreach(_.foreachNode(a => c.add(a.uuid)))
    c
  }
}
