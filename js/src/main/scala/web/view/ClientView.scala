package web.view

import client.Client
import org.scalajs.dom.window
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw._
import util.ObservableProperty

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

// in this class we use nulls for a various things, but not for public API
class ClientView(private val parent: html.Element, private val client: Client) {

  private val des = ArrayBuffer[Unit => Unit]()

  def destroy(): Unit = {
    des.reverse.foreach(_.apply())
  }

  // initialise
  private val rootView = dom.document.createElement("div").asInstanceOf[HTMLDivElement]
  parent.appendChild(rootView)
  rootView.contentEditable = "true"
  des.append(_ => parent.removeChild(rootView))


  private var shiftKey = false
  private var mouseDown: MouseDown = null
  private var inDomChange: DomChange = null
  private var lastKeyCode: Integer = null
  private var lastKeyCodeTime: Long = 0
  private val domObserver = new DomObserver()

  /**
    *
    *
    *
    * dom observer
    *
    *
    *
    */

  domObserver.start()
  des.append(_ => domObserver.stop())

  class DomObserver {
    private val mutationObserverOptions = MutationObserverInit(
      childList = true,
      characterData = true,
      attributes = true,
      subtree = true,
      characterDataOldValue = true)
    private val mutationObserver = new MutationObserver((changes, obs) => {
      registerMutations(changes)
    })

    def start(): Unit = {
      mutationObserver.observe(rootView, mutationObserverOptions)
    }


    def flush(): Unit = {
      registerMutations(mutationObserver.takeRecords())
    }

    def stop(): Unit = {
      flush()
      mutationObserver.disconnect()
    }

    private def registerMutations(value: js.Array[MutationRecord]): Unit = {
      // TODO what's the purpose of this?
    }
  }


  /**
    *
    *
    * dom change
    *
    *
    */

  des.append(_ => if (inDomChange != null) inDomChange.destroy())

  val commitTimeout = 20
  class DomChange(
    var composing: Boolean = false
  ) {

    var timeout: Integer = if (composing) null else window.setTimeout(() => finish(), commitTimeout)


    def destroy(): Unit = {

    }

    def finish(): Unit = {

      // TODO
    }
    def compositionEnd(): Unit = {
      // TODO
    }

  }


  def startDomChange(composing: Boolean = false): DomChange = {
    if (inDomChange != null) {
      if (composing) {
        if (inDomChange.timeout != null) window.clearTimeout(inDomChange.timeout)
        inDomChange.composing = true
      }
    } else {
      inDomChange = new DomChange(composing)
    }
    inDomChange
  }

  /**
    *
    * event helper
    *
    */

  def event[T <: Event](`type`: String,
    listener: js.Function1[T, _]): Unit = {
    rootView.addEventListener(`type`, listener)
    des.append(_ => rootView.removeEventListener(`type`, listener))
  }

  def editEvent[T <: Event](`type`: String,
    listener: js.Function1[T, _]): Unit = {
    rootView.addEventListener(`type`, listener)
    des.append(_ => rootView.removeEventListener(`type`, listener))
  }

  /**
    *
    *
    *
    * keyboard
    *
    *
    */

  editEvent("keydown", (a: KeyboardEvent) => {
    println(a)
  })

  editEvent("keyup", (a: KeyboardEvent) => {
    println(a)
  })

  editEvent("keypress", (a: KeyboardEvent) => {
  })


  /**
    *
    *
    * input
    *
    *
    */

  editEvent("compositionstart", (a: CompositionEvent) => {
    startDomChange(true)
  })

  editEvent("compositionupdate", (a: CompositionEvent) => {
    startDomChange(true)
  })

  editEvent("compositionend", (a: CompositionEvent) => {
    if (inDomChange == null) {
      if (a.data != null) startDomChange(true)
    }
    if (inDomChange != null) inDomChange.compositionEnd()
  })

  editEvent("input", (a: Event) => {
    val change = startDomChange()
    if (!change.composing) change.finish()
  })

  /**
    *
    *
    * mouse
    *
    *
    */

  event("mousedown", (a: MouseEvent) => {
    println(a)
  })


  class MouseDown {

  }

  event("contextmenu", (a: MouseEvent) => {
    println(a)
  })


  /***
    *
    *
    * copy paste
    *
    *
    */

  editEvent("copy", (a: ClipboardEvent) => {
    println(a)
  })

  editEvent("cut", (a: ClipboardEvent) => {
    println(a)
  })

  editEvent("paste", (a: ClipboardEvent) => {
    println(a)
  })


  /**
    *
    *
    * drag drop
    *
    *
    */

  event("dragstart", (a: DragEvent) => {
    println(a)
  })

  event("dragend", (a: DragEvent) => {
    println(a)
  })

  editEvent("dragover", (a: DragEvent) => {
    println(a)
  })

  editEvent("dragenter", (a: DragEvent) => {
    println(a)
  })

  editEvent("drop", (a: DragEvent) => {
    println(a)
  })


  /**
    *
    *
    * focus events
    *
    *
    */

  event("focus", (a: FocusEvent) => {
    println(a)
  })

  event("blur", (a: FocusEvent) => {
    println(a)
  })


  /****
    *
    *
    *
    *
    *
    * selection reader
    *
    *
    */

  private val selectionReader = new SelectionReader()
  class SelectionReader() {

    des.append(_ => destroy())
  }

}
