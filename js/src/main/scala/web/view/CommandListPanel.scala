package web.view

import client.Client
import command.Key
import scalatags.JsDom.all._
import org.scalajs.dom.raw.HTMLElement

class CommandListPanel(val client: Client) extends UnselectableView  {


  dom = div(
    minWidth := "150px",
    width := "100%",
    height := "100%",
    color := "#cccccc",
    overflowY := "scroll",
    `class` := "ct-scroll",
    padding := "24px",
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
                  p(marginLeft := "12px",
                    Some(span(tag("kbd")(`class` := "ct-kbd2", "N"), " ")).filter(_ => c.repeatable),
                    c.keys.map(a => span(tag("kbd")(`class` := "ct-kbd", Key.toString(a)), " ")),
                    Some(span(tag("kbd")(`class` := "ct-kbd2", "char"), " ")).filter(_ => c.needsChar),
                    Some(span(tag("kbd")(`class` := "ct-kbd2", "motion"), " ")).filter(_ => c.needsMotion),
                    c.description)
                )
              })
            )
          }
      }.toSeq
    ).render
//    dom.appendChild(div(
//      client.debug_undoHistory.map(a => p(a)).toSeq
//    ).render)
    dom.appendChild(res)
    dom.childNodes(0).asInstanceOf[HTMLElement].scrollIntoView(true)
  }))

}