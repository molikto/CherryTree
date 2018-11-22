package web.ui.content
import view.EditorInterface
import web.ui.content.ContentViewEditor.General
import web.ui.doc.DocumentView

private [content] trait StaticCodeView extends ContentView.Code {
  override def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General = throw new NotImplementedError("Not possible")
}
