package forms

import play.api.data.Forms._
import play.api.data._

/**
 * The `Forgot Password` form.
 */
object CollabratorForm {

  /**
   * A play framework form.
   */
  val form = Form(
    mapping(
      "email" -> email,
      "level" -> shortNumber
    )(Data.apply)(Data.unapply)
  )

  case class Data(
               email: String,
               level: Short)
}
