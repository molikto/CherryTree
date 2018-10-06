package controllers.user

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.util.{Credentials, PasswordHasherRegistry, PasswordInfo}
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import controllers.{AssetsFinder, routes}
import forms.ChangePasswordForm
import javax.inject.Inject
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents}
import utils.auth.{DefaultEnv, WithProvider}

import scala.concurrent.{ExecutionContext, Future}

class ChangePasswordController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  credentialsProvider: CredentialsProvider,
  authInfoRepository: AuthInfoRepository,
  passwordHasherRegistry: PasswordHasherRegistry
)(
  implicit
  assets: AssetsFinder,
  ex: ExecutionContext
) extends AbstractController(components) with I18nSupport {

  def view = silhouette.SecuredAction(WithProvider[DefaultEnv#A](CredentialsProvider.ID)) {
    implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
      Ok(views.html.changePassword(ChangePasswordForm.form, request.identity))
  }

  def submit = silhouette.SecuredAction(WithProvider[DefaultEnv#A](CredentialsProvider.ID)).async {
    implicit request: SecuredRequest[DefaultEnv, AnyContent] =>
      ChangePasswordForm.form.bindFromRequest.fold(
        form => Future.successful(BadRequest(views.html.changePassword(form, request.identity))),
        password => {
          val (currentPassword, newPassword) = password
          val credentials = Credentials(request.identity.email.getOrElse(""), currentPassword)
          credentialsProvider.authenticate(credentials).flatMap { loginInfo =>
            val passwordInfo = passwordHasherRegistry.current.hash(newPassword)
            authInfoRepository.update[PasswordInfo](loginInfo, passwordInfo).map { _ =>
              Redirect(routes.ChangePasswordController.view()).flashing("success" -> Messages("password.changed"))
            }
          }.recover {
            case _: ProviderException =>
              Redirect(routes.ChangePasswordController.view()).flashing("error" -> Messages("current.password.invalid"))
          }
        }
      )
  }
}
