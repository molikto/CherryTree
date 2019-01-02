package web.ui.content

import model._
import scalatags.JsDom.all._
import view.EditorInterface
import web.ui
import web.ui.doc.{DocumentView, LaTeXMacroCache}

private [content] class WrappedCodeView(initData: model.data.Content.Code,
                                        override val latexMacroCache: LaTeXMacroCache
                                               ) extends ContentView.Code with ContentViewCreator  {


  private var codeView: ContentView.Code = conentViewFromCode(initData)


  override def tempEditableTempDuringSelectionChange(editable: Boolean): Unit = {
    //dom.contentEditable = editable.toString
  }

  protected override def onUpdateContent(contentData: model.data.Content.Code): Unit = {
    if (contentViewMatches(contentData.lang, codeView)) {
      codeView.updateContent(contentData)
    } else {
      codeView.destroy()
      codeView = conentViewFromCode(contentData)
      codeView.attachToNode(dom)
    }
  }


  override def destroy(): Unit = {
    codeView.destroy()
    super.destroy()
  }

  protected override def onUpdateContent(c: model.data.Content.Code, trans: operation.Content.Code, viewUpdated: Boolean): Unit = {
    if (contentViewMatches(c.lang, codeView)) {
      codeView.updateContent(c, trans, viewUpdated)
    } else {
      codeView.destroy()
      codeView = conentViewFromCode(c)
      codeView.attachToNode(dom)
    }
  }

  dom = div(
    position := "relative",
    span(
      position := "absolute",
      ui.EvilChar, // so we can get selection during evil time!
    )
  ).render

  override def onAttach(): Unit = {
    super.onAttach()
    codeView.attachToNode(dom)
  }

  override def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General =
    new CodeViewEditor(documentView, controller, this).asInstanceOf[ContentViewEditor.General]
  setInitialContent(initData)

}
