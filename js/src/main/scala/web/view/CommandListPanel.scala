package web.view

import client.Client
import command.{Command, Key}
import scalatags.JsDom.all._
import org.scalajs.dom.raw.{HTMLElement, MouseEvent}

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

class CommandListPanel(val client: Client, doc: () => View) extends UnselectableView  {


  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    overflowY := "scroll",
    `class` := "ct-scroll ct-panel",
    padding := "24px",
  ).render

  private val bf = new ArrayBuffer[(Command, HTMLElement)]

  private val data = client.commandsByCategory.map { case (name, cs) => (name, cs.filter(a => a.description.nonEmpty) )}


  private val onClick: js.Function1[MouseEvent, _] = (ev: MouseEvent) => {
    val cmd = ev.currentTarget.asInstanceOf[js.Dynamic].command.asInstanceOf[command.Command]
    client.runTextualIfAvailable(cmd)
    doc().focus()
  }

  private val onPanelClick: js.Function1[MouseEvent, _] = (ev: MouseEvent) => {
    ev.currentTarget.asInstanceOf[HTMLElement].parentElement.classList.toggle("hide-children")
  }

  val res = div(
    data.map {
      case (name, commands) =>
        if (commands.isEmpty) {
          div()
        } else {
          div(
            marginBottom := "8px",
            {
              val h = h4(
                `class` := "ct-flat-selectable",
                marginLeft := "-6px",
                marginRight := "-6px",
                marginBottom := "0px",
                paddingLeft := "6px",
                paddingRight := "6px",
                paddingTop := "4px",
                paddingBottom := "4px",
                name).render
              h.addEventListener("click", onPanelClick)
              h: Frag
            },
            commands.map(c => {
              val dom = div(
                p(marginLeft := "8px",
                  marginBottom := "0px",
                  paddingLeft := "4px",
                  paddingRight := "4px",
                  paddingTop := "5px",
                  paddingBottom := "5px",
                  Some(span(tag("kbd")(`class` := "ct-kbd2", "N"), " ")).filter(_ => c.repeatable),
                  c.keys.map(a => span(tag("kbd")(`class` := "ct-kbd", Key.toString(a)), " ")),
                  Some(span(tag("kbd")(`class` := "ct-kbd2", "char"), " ")).filter(_ => c.needsChar),
                  Some(span(tag("kbd")(`class` := "ct-kbd2", "motion"), " ")).filter(_ => c.needsMotion),
                  c.description)
              ).render
              dom.asInstanceOf[js.Dynamic].command = c.asInstanceOf[scala.scalajs.js.Any]
              if (!c.needsStuff) {
                dom.className = "ct-flat-selectable"
                dom.addEventListener("click", onClick)
              }
              bf.append((c, dom))
              dom: Frag
            })
          )
        }
    }.toSeq
  ).render

  dom.appendChild(res)


  observe(client.stateUpdates.map(_ => 0).startWith(Seq(0)).doOnNext(pair => {
    for (p <- bf) {
      val av = p._1.available(client.state, client)
      if (av) {
        p._2.classList.remove("ct-flat-disabled")
      } else {
        p._2.classList.add("ct-flat-disabled")
      }
    }
  }))

}
