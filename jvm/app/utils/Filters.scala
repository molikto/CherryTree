package utils

import akka.http.scaladsl.model.ContentType
import javax.inject.Inject
import play.api.Configuration
import play.api.http.{ContentTypes, HttpFilters, MimeTypes}
import play.api.mvc.{EssentialAction, EssentialFilter, RequestHeader, Result}
import play.filters.csrf.CSRFFilter
import play.filters.headers.{SecurityHeadersConfig, SecurityHeadersFilter}
import play.twirl.api.Html

/**
 * Provides filters.
 */
class Filters @Inject() (csrfFilter: CSRFFilter, configuration: Configuration) extends HttpFilters {

  val securityHeadersFilter = new SecurityHeadersFilter(SecurityHeadersConfig.fromConfiguration(configuration)) {
    override protected def headers(request: RequestHeader, result: Result): Seq[(String, String)] = {
      if (result.body.contentType.exists(_.startsWith(MimeTypes.HTML))) {
        super.headers(request, result)
      } else {
        Seq.empty
      }
    }
  }

  override def filters: Seq[EssentialFilter] = Seq(csrfFilter, securityHeadersFilter)
}
