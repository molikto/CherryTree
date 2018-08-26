package web.ui.content
import view.EditorInterface
import web.ui.content.ContentViewEditor.General
import web.ui.doc.{AbstractDocumentView, DocumentView}

trait StaticCodeView extends ContentView.Code {
  override def createEditor(documentView: AbstractDocumentView, controller: EditorInterface): ContentViewEditor.General = throw new NotImplementedError("Not possible")
}
