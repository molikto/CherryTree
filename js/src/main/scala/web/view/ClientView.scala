package web.view

import client.Client
import org.scalajs.dom.html
import org.scalajs.dom.raw._

import scala.scalajs.js

// in this class we use nulls for a various things, but not for public API
class ClientView(private val rootView: html.Element, private val client: Client) {


  // initialise
  rootView.contentEditable = "true"

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

    private def registerMutations(value: js.Array[MutationRecord]) = {
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

  class DomChange {

    var composing: Boolean = false

    def finish(): Unit = {

      // TODO
    }
    def compositionEnd(): Unit = {
      // TODO
    }

  }


  def startDomChange(composing: Boolean = false): DomChange = {
    // TODO
    ???
  }


  /**
    *
    *
    *
    * keyboard
    *
    *
    */

  rootView.addEventListener("keydown", (a: KeyboardEvent) => {
    println(a)
  })

  rootView.addEventListener("keyup", (a: KeyboardEvent) => {
    println(a)
  })

  rootView.addEventListener("keypress", (a: KeyboardEvent) => {
  })


  /**
    *
    *
    * mouse
    *
    *
    */

  rootView.addEventListener("mousewown", (a: MouseEvent) => {
    println(a)
  })


  class MouseDown {

  }

  rootView.addEventListener("contextmenu", (a: MouseEvent) => {
    println(a)
  })


  /**
    *
    *
    * input
    *
    *
    */

  rootView.addEventListener("compositionstart", (a: CompositionEvent) => {
    startDomChange(true)
  })

  rootView.addEventListener("compositionupdate", (a: CompositionEvent) => {
    startDomChange(true)
  })

  rootView.addEventListener("compositionend", (a: CompositionEvent) => {
    if (inDomChange == null) {
      if (a.data != null) startDomChange(true)
    }
    if (inDomChange != null) inDomChange.compositionEnd()
  })

  rootView.addEventListener("input", (a: Event) => {
    val change = startDomChange()
    if (!change.composing) change.finish()
  })


  /***
    *
    *
    * copy paste
    *
    *
    */

  rootView.addEventListener("copy", (a: ClipboardEvent) => {
    println(a)
  })

  rootView.addEventListener("cut", (a: ClipboardEvent) => {
    println(a)
  })

  rootView.addEventListener("paste", (a: ClipboardEvent) => {
    println(a)
  })


  /**
    *
    *
    * drag drop
    *
    *
    */

  rootView.addEventListener("dragstart", (a: DragEvent) => {
    println(a)
  })

  rootView.addEventListener("dragend", (a: DragEvent) => {
    println(a)
  })

  rootView.addEventListener("dragover", (a: DragEvent) => {
    println(a)
  })

  rootView.addEventListener("drop", (a: DragEvent) => {
    println(a)
  })


  /**
    *
    *
    * focus events
    *
    *
    */

  rootView.addEventListener("focus", (a: FocusEvent) => {
    println(a)
  })

  rootView.addEventListener("blur", (a: FocusEvent) => {
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

  }

}
