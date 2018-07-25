package web.view

import client.Client

import scalatags.JsDom.all._
import org.scalajs.dom.raw.HTMLElement

class CommandListView(val client: Client) extends View {


  dom = div(
    minWidth := "150px",
    width := "500px",
    height := "100%",
    color := "#dddddd",
    overflowY := "scroll",
    `class` := "ct-scroll unselectable",
    padding := "24px",
    background := theme.bottomBarBackground
  ).render


  observe(client.stateUpdates.map(_ => 0).startWith(Seq(0)).map(_ => {
    client.commandsByCategory.map {
      case (name, cs) =>
        (name, cs.filter(a => a.available(client.state, client) && a.description.nonEmpty && a.keys.nonEmpty))
    }
  }).distinctUntilChanged.doOnNext(pair => {
    removeAllChild(dom)
    val res = div(
      pair.map {
        case (name, commands) =>
          if (commands.isEmpty) {
            div()
          } else {
            div(
              h4(name),
              commands.map(c => {
                div(
                  p(marginLeft := "12px", c.keys.map(a => span(tag("kbd")(renderKeySeq(a)), " ")), c.description, Some(span("; ", em("repeatable"))).filter(_ => c.repeatable))
                )
              })
            )
          }
      }.toSeq
    ).render
    dom.appendChild(res)
    dom.scrollIntoView(true)
  }))
}
