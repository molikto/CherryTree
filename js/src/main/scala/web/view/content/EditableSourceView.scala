package web.view.content
import model.data.Content
import view.EditorInterface
import web.view.doc.DocumentView

class EditableSourceView(
  override val documentView: DocumentView,
  override val controller: EditorInterface,
  c0: model.data.Content.Code
) extends SourceView(c0) with EditableCodeView {
}
