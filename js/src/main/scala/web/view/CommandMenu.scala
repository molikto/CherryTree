package web.view

import client.Client
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import scalatags.JsDom.all._
import web.view.doc.DocumentView

class CommandMenu(val client: Client) extends View {

  private val search = input().render

  private val list = div().render

  private val menu = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    width := "100px",
    height := "100px",
    border := "1px solid #474a4f",
    background := "#202328",
    search,
    list
  ).render


  dom = div(
    position := "absolute",
    left := "0px",
    top := "0px",
    width := "100%",
    height := "100%",
    zIndex := "100",
    menu,
  ).render

  var available: Seq[command.Command] = Seq.empty

  observe(client.stateUpdates.doOnNext(u => {
    if (available.nonEmpty) {
      // update command list
      updateMenuContent()
    }
  }))

  def updateMenuContent(): Unit = {
    val n = client.commands.filter(a => a.available(client.state, client) && a.keys.isEmpty)
    if (n != available) {
      available = n
      if (available.isEmpty) {
        dismiss()
      } else {
        removeAllChild(list)
        for (c <- available) {
          list.appendChild(p(c.description).render)
        }
      }
    }
  }

  def showAt(x: Float, y: Float): Unit = {
    menu.style.left = x.toString
    menu.style.top = y.toString
    dom.style.display = "block"
    search.textContent = ""
    search.focus()
    updateMenuContent()
  }

  def dismiss(): Unit = {
    available = Seq.empty
    dom.style.display = "none"
  }

  dismiss()
}
