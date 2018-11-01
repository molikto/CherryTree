package model

import play.api.libs.json.Format

trait Formats {



  implicit def format_Node: Format[data.Node] = data.Node.jsonFormat

  implicit def format_Content: Format[data.Content] = data.Content.jsonFormat

  implicit def format_Paragraph: Format[data.Rich] = data.Rich.jsonFormat

  implicit def format_Unicode: Format[data.Unicode] = data.Unicode.jsonFormat

  implicit def format_EncodedSeq: Format[data.EncodedSeq] = data.EncodedSeq.jsonFormat

}
