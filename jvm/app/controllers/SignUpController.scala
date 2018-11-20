package controllers


import java.util.UUID

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.AvatarService
import com.mohiva.play.silhouette.api.util.PasswordHasherRegistry
import com.mohiva.play.silhouette.impl.providers._
import forms.SignUpForm
import javax.inject.Inject
import models.User
import play.api.i18n.{I18nSupport, Messages}
import play.api.libs.json.{JsSuccess, Json}
import play.api.libs.mailer.{Email, MailerClient}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request}
import repos.{AuthTokenRepository, UserRepository}
import utils.auth.DefaultEnv

import scala.concurrent.{ExecutionContext, Future}

class SignUpController @Inject() (
  components: ControllerComponents,
  silhouette: Silhouette[DefaultEnv],
  userService: UserRepository,
  authInfoRepository: AuthInfoRepository,
  authTokenService: AuthTokenRepository,
  avatarService: AvatarService,
  passwordHasherRegistry: PasswordHasherRegistry,
  mailerClient: MailerClient
)(
  implicit
  assets: AssetsFinder,
  ex: ExecutionContext
) extends AbstractController(components) with I18nSupport {

  def view = silhouette.UnsecuredAction.async { implicit request: Request[AnyContent] =>
    Future.successful(Ok(views.html.signUp(SignUpForm.form)))
  }

  def submit = silhouette.UnsecuredAction.async { implicit request: Request[AnyContent] =>
    SignUpForm.form.bindFromRequest.fold(
      form => Future.successful(BadRequest(views.html.signUp(form))),
      data => {
        val result = Redirect(routes.SignInController.view()).flashing("info" -> Messages("sign.up.email.sent", data.email))
        val loginInfo = LoginInfo(CredentialsProvider.ID, data.email)
        userService.retrieve(loginInfo).flatMap {
          case Some(user) =>
            val url = routes.SignInController.view().absoluteURL()
            mailerClient.send(Email(
              subject = Messages("email.already.signed.up.subject"),
              from = Messages("email.from"),
              to = Seq(data.email),
              bodyText = Some(views.txt.emails.alreadySignedUp(user, url).body),
              bodyHtml = Some(views.html.emails.alreadySignedUp(user, url).body)
            ))

            Future.successful(result)
          case None =>
            val authInfo = passwordHasherRegistry.current.hash(data.password)
            val Allowed = Seq("molikto@gmail.com","nirrrh@gmail.com", "wuthefwasthat@gmail.com", "revolution06@foxmail.com", "wtf@gmail.com", "hotterd@gmail.com", "zhengt.cn@gmail.com", "ikenchina@gmail.com", "zhengxiao.cn@gmail.com", "hectorinsane@gmail.com", "dage1357@gmail.com")
            if (Allowed.contains(data.email)) {
              userService.retrieve(data.email).flatMap {
                case Some(u) =>
                  Future.successful(Redirect(routes.SignInController.view()).flashing("error" -> Messages("email.taken")))
                case None =>
                  val user0 = User(
                    userId = UUID.randomUUID(),
                    createdTime = 0,
                    name = data.name,
                    email = data.email,
                    avatarUrl = None,
                    activated = true, // TODO remove the allowed stuff and actually send email
                    loginInfo = loginInfo
                  )
                  for {
                    avatar <- avatarService.retrieveURL(data.email)
                    user <- userService.create(user0.copy(avatarUrl = avatar), authInfo, userService.newUserDocument())
                    authToken <- authTokenService.create(user.userId)
                  } yield {
                    val url = routes.ActivateAccountController.activate(authToken.id).absoluteURL()
                    mailerClient.send(Email(
                      subject = Messages("email.sign.up.subject"),
                      from = Messages("email.from"),
                      to = Seq(data.email),
                      bodyText = Some(views.txt.emails.signUp(user, url).body),
                      bodyHtml = Some(views.html.emails.signUp(user, url).body)
                    ))

                    silhouette.env.eventBus.publish(SignUpEvent(user, request))
                    result
                  }
              }
            } else {
              Future.successful(Redirect(routes.SignUpController.view()).flashing("error" -> "We are currently in closed-alpha and only whitelisted user can signup."))
            }
        }
      }
    )
  }
}
