package web.view.content
import view.EditorInterface
import web.view.content.ContentViewEditor.General
import web.view.doc.DocumentView

trait StaticCodeView extends ContentView.Code {
  override def createEditor(documentView: DocumentView, controller: EditorInterface): ContentViewEditor.General = throw new NotImplementedError("Not possible")
}
