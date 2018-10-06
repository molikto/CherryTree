package modules

import com.google.inject.name.Named
import com.google.inject.{AbstractModule, Provides}
import com.mohiva.play.silhouette.api.actions.{SecuredErrorHandler, UnsecuredErrorHandler}
import com.mohiva.play.silhouette.api.crypto._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services._
import com.mohiva.play.silhouette.api.util._
import com.mohiva.play.silhouette.api.{Environment, EventBus, Silhouette, SilhouetteProvider}
import com.mohiva.play.silhouette.crypto.{JcaCrypter, JcaCrypterSettings, JcaSigner, JcaSignerSettings}
import com.mohiva.play.silhouette.impl.authenticators._
import com.mohiva.play.silhouette.impl.providers._
import com.mohiva.play.silhouette.impl.providers.oauth1._
import com.mohiva.play.silhouette.impl.providers.oauth1.secrets.{CookieSecretProvider, CookieSecretSettings}
import com.mohiva.play.silhouette.impl.providers.oauth1.services.PlayOAuth1Service
import repos._
import com.mohiva.play.silhouette.impl.providers.oauth2._
import com.mohiva.play.silhouette.impl.providers.openid.YahooProvider
import com.mohiva.play.silhouette.impl.providers.openid.services.PlayOpenIDService
import com.mohiva.play.silhouette.impl.providers.state.{CsrfStateItemHandler, CsrfStateSettings}
import com.mohiva.play.silhouette.impl.services._
import com.mohiva.play.silhouette.impl.util._
import com.mohiva.play.silhouette.password.{BCryptPasswordHasher, BCryptSha256PasswordHasher}
import com.mohiva.play.silhouette.persistence.daos.{DelegableAuthInfoDAO, InMemoryAuthInfoDAO}
import com.mohiva.play.silhouette.persistence.repositories.DelegableAuthInfoRepository
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.ValueReader
import net.codingwell.scalaguice.ScalaModule
import play.api.Configuration
import play.api.libs.openid.OpenIdClient
import play.api.libs.ws.WSClient
import play.api.mvc.{Cookie, CookieHeaderEncoding}
import utils.auth.{CustomSecuredErrorHandler, CustomUnsecuredErrorHandler, DefaultEnv}

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * The Guice module which wires all Silhouette dependencies.
 */
class UserModule extends AbstractModule with ScalaModule {

  implicit val sameSiteReader: ValueReader[Option[Option[Cookie.SameSite]]] =
    (config: Config, path: String) => {
      if (config.hasPathOrNull(path)) {
        if (config.getIsNull(path))
          Some(None)
        else {
          Some(Cookie.SameSite.parse(config.getString(path)))
        }
      } else {
        None
      }
    }

  def configure() {
    bind[Silhouette[DefaultEnv]].to[SilhouetteProvider[DefaultEnv]]
    bind[UnsecuredErrorHandler].to[CustomUnsecuredErrorHandler]
    bind[SecuredErrorHandler].to[CustomSecuredErrorHandler]
    bind[CacheLayer].to[PlayCacheLayer]
    bind[IDGenerator].toInstance(new SecureRandomIDGenerator())
    bind[FingerprintGenerator].toInstance(new DefaultFingerprintGenerator(false))
    bind[EventBus].toInstance(EventBus())
    bind[Clock].toInstance(Clock())

    // Replace this with the bindings to your concrete DAOs
    bind[DelegableAuthInfoDAO[PasswordInfo]].to[PasswordInfoDAO]
    bind[DelegableAuthInfoDAO[OAuth1Info]].to[OAuth1InfoDAO]
    bind[DelegableAuthInfoDAO[OAuth2Info]].to[OAuth2InfoDAO]
    bind[DelegableAuthInfoDAO[OpenIDInfo]].to[OpenIDInfoDAO]
 }

  @Provides
  def provideHTTPLayer(client: WSClient): HTTPLayer = new PlayHTTPLayer(client)

  @Provides
  def provideEnvironment(
    userService: UserRepository,
    authenticatorService: AuthenticatorService[CookieAuthenticator],
    eventBus: EventBus): Environment[DefaultEnv] = {

    Environment[DefaultEnv](
      userService,
      authenticatorService,
      Seq(),
      eventBus
    )
  }

  @Provides
  def provideSocialProviderRegistry(
    facebookProvider: FacebookProvider,
    googleProvider: GoogleProvider,
    vkProvider: VKProvider,
    twitterProvider: TwitterProvider,
    xingProvider: XingProvider,
    yahooProvider: YahooProvider): SocialProviderRegistry = {

    SocialProviderRegistry(Seq(
      googleProvider,
      facebookProvider,
      twitterProvider,
      vkProvider,
      xingProvider,
      yahooProvider
    ))
  }

  @Provides @Named("oauth1-token-secret-signer")
  def provideOAuth1TokenSecretSigner(configuration: Configuration): Signer = {
    val config = configuration.underlying.as[JcaSignerSettings]("silhouette.oauth1TokenSecretProvider.signer")

    new JcaSigner(config)
  }

  @Provides @Named("oauth1-token-secret-crypter")
  def provideOAuth1TokenSecretCrypter(configuration: Configuration): Crypter = {
    val config = configuration.underlying.as[JcaCrypterSettings]("silhouette.oauth1TokenSecretProvider.crypter")

    new JcaCrypter(config)
  }

  @Provides @Named("csrf-state-item-signer")
  def provideCSRFStateItemSigner(configuration: Configuration): Signer = {
    val config = configuration.underlying.as[JcaSignerSettings]("silhouette.csrfStateItemHandler.signer")

    new JcaSigner(config)
  }

  @Provides @Named("social-state-signer")
  def provideSocialStateSigner(configuration: Configuration): Signer = {
    val config = configuration.underlying.as[JcaSignerSettings]("silhouette.socialStateHandler.signer")

    new JcaSigner(config)
  }

  @Provides @Named("authenticator-signer")
  def provideAuthenticatorSigner(configuration: Configuration): Signer = {
    val config = configuration.underlying.as[JcaSignerSettings]("silhouette.authenticator.signer")

    new JcaSigner(config)
  }

  @Provides @Named("authenticator-crypter")
  def provideAuthenticatorCrypter(configuration: Configuration): Crypter = {
    val config = configuration.underlying.as[JcaCrypterSettings]("silhouette.authenticator.crypter")

    new JcaCrypter(config)
  }

  @Provides
  def provideAuthInfoRepository(
    passwordInfoDAO: DelegableAuthInfoDAO[PasswordInfo],
    oauth1InfoDAO: DelegableAuthInfoDAO[OAuth1Info],
    oauth2InfoDAO: DelegableAuthInfoDAO[OAuth2Info],
    openIDInfoDAO: DelegableAuthInfoDAO[OpenIDInfo]): AuthInfoRepository = {

    new DelegableAuthInfoRepository(passwordInfoDAO, oauth1InfoDAO, oauth2InfoDAO, openIDInfoDAO)
  }

  @Provides
  def provideAuthenticatorService(
    @Named("authenticator-signer") signer: Signer,
    @Named("authenticator-crypter") crypter: Crypter,
    cookieHeaderEncoding: CookieHeaderEncoding,
    fingerprintGenerator: FingerprintGenerator,
    idGenerator: IDGenerator,
    configuration: Configuration,
    clock: Clock): AuthenticatorService[CookieAuthenticator] = {

    val config = configuration.underlying.as[CookieAuthenticatorSettings]("silhouette.authenticator")
    val authenticatorEncoder = new CrypterAuthenticatorEncoder(crypter)

    new CookieAuthenticatorService(config, None, signer, cookieHeaderEncoding, authenticatorEncoder, fingerprintGenerator, idGenerator, clock)
  }

  @Provides
  def provideAvatarService(httpLayer: HTTPLayer): AvatarService = new GravatarService(httpLayer)

  @Provides
  def provideOAuth1TokenSecretProvider(
    @Named("oauth1-token-secret-signer") signer: Signer,
    @Named("oauth1-token-secret-crypter") crypter: Crypter,
    configuration: Configuration,
    clock: Clock): OAuth1TokenSecretProvider = {

    val settings = configuration.underlying.as[CookieSecretSettings]("silhouette.oauth1TokenSecretProvider")
    new CookieSecretProvider(settings, signer, crypter, clock)
  }

  @Provides
  def provideCsrfStateItemHandler(
    idGenerator: IDGenerator,
    @Named("csrf-state-item-signer") signer: Signer,
    configuration: Configuration): CsrfStateItemHandler = {
    val settings = configuration.underlying.as[CsrfStateSettings]("silhouette.csrfStateItemHandler")
    new CsrfStateItemHandler(settings, idGenerator, signer)
  }

  @Provides
  def provideSocialStateHandler(
    @Named("social-state-signer") signer: Signer,
    csrfStateItemHandler: CsrfStateItemHandler): SocialStateHandler = {

    new DefaultSocialStateHandler(Set(csrfStateItemHandler), signer)
  }

  @Provides
  def providePasswordHasherRegistry(): PasswordHasherRegistry = {
    PasswordHasherRegistry(new BCryptSha256PasswordHasher(), Seq(new BCryptPasswordHasher()))
  }

  @Provides
  def provideCredentialsProvider(
    authInfoRepository: AuthInfoRepository,
    passwordHasherRegistry: PasswordHasherRegistry): CredentialsProvider = {

    new CredentialsProvider(authInfoRepository, passwordHasherRegistry)
  }

  @Provides
  def provideFacebookProvider(
    httpLayer: HTTPLayer,
    socialStateHandler: SocialStateHandler,
    configuration: Configuration): FacebookProvider = {

    new FacebookProvider(httpLayer, socialStateHandler, configuration.underlying.as[OAuth2Settings]("silhouette.facebook"))
  }

  @Provides
  def provideGoogleProvider(
    httpLayer: HTTPLayer,
    socialStateHandler: SocialStateHandler,
    configuration: Configuration): GoogleProvider = {

    new GoogleProvider(httpLayer, socialStateHandler, configuration.underlying.as[OAuth2Settings]("silhouette.google"))
  }

  @Provides
  def provideVKProvider(
    httpLayer: HTTPLayer,
    socialStateHandler: SocialStateHandler,
    configuration: Configuration): VKProvider = {

    new VKProvider(httpLayer, socialStateHandler, configuration.underlying.as[OAuth2Settings]("silhouette.vk"))
  }

  @Provides
  def provideTwitterProvider(
    httpLayer: HTTPLayer,
    tokenSecretProvider: OAuth1TokenSecretProvider,
    configuration: Configuration): TwitterProvider = {

    val settings = configuration.underlying.as[OAuth1Settings]("silhouette.twitter")
    new TwitterProvider(httpLayer, new PlayOAuth1Service(settings), tokenSecretProvider, settings)
  }

  @Provides
  def provideXingProvider(
    httpLayer: HTTPLayer,
    tokenSecretProvider: OAuth1TokenSecretProvider,
    configuration: Configuration): XingProvider = {

    val settings = configuration.underlying.as[OAuth1Settings]("silhouette.xing")
    new XingProvider(httpLayer, new PlayOAuth1Service(settings), tokenSecretProvider, settings)
  }

  @Provides
  def provideYahooProvider(
    httpLayer: HTTPLayer,
    client: OpenIdClient,
    configuration: Configuration): YahooProvider = {

    val settings = configuration.underlying.as[OpenIDSettings]("silhouette.yahoo")
    new YahooProvider(httpLayer, new PlayOpenIDService(client, settings), settings)
  }
}
