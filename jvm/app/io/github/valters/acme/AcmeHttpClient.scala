/*
  Copyright 2017 Valters Vingolds

  This file is licensed to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

package io.github.valters.acme

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import com.typesafe.scalalogging.Logger
import play.api.libs.ws.WSClient
import play.api.libs.ws.WSResponse
import javax.inject.Inject
import java.security.cert.X509Certificate

import akka.util.ByteString
import java.util.concurrent.TimeUnit

import scala.concurrent.Promise
import com.google.common.net.HttpHeaders
import com.google.inject.ImplementedBy

@ImplementedBy(classOf[AcmeHttpClientImpl])
trait AcmeHttpClient {
  def takeNonce(): String

  def getDirectory(endpoint: String ): Future[AcmeProtocol.Directory]

  def registration( uri: String, message: String  ): Future[AcmeProtocol.SimpleRegistrationResponse]

  def authorize( uri: String, message: String  ): Future[AcmeProtocol.AuthorizationResponse]

  def agreement( uri: String, message: String ): Future[AcmeProtocol.RegistrationResponse]

  def challenge( uri: String, message: String ): Future[AcmeProtocol.ChallengeType]

  def challengeDetails(uri: String): Future[AcmeProtocol.ChallengeType]

  def issue( uri: String, message: String ): Future[X509Certificate]

  val acmeServer: Promise[AcmeProtocol.AcmeServer] = Promise[AcmeProtocol.AcmeServer]()
}

/** extract few fields of interest from the underlying http WSResponse */
final case class Response( status: Int, body: String, headers: Map[String, Seq[String]], nonce: Option[String] ) {
  def this( resp: WSResponse ) {
    this( resp.status, resp.body, resp.allHeaders, resp.header( AcmeProtocol.NonceHeader ) )
  }
}

/** binary response */
final case class BResponse( status: Int, body: ByteString, headers: Map[String, Seq[String]], nonce: Option[String] ) {
  def this( resp: WSResponse ) {
    this( resp.status, resp.bodyAsBytes, resp.allHeaders, resp.header( AcmeProtocol.NonceHeader ) )
  }
}

class AcmeHttpClientImpl @Inject() (wsClient: WSClient) extends AcmeHttpClient {
  private val logger = Logger[AcmeHttpClient]

  /** default request content type */
  private val MimeUrlencoded = "application/x-www-form-urlencoded"

  /** request the certificate in specific format that Java likes */
  private val AcceptMimeCert = "application/pkix-cert"

  /** track down the links that ACME advertises in response */
  private val HeaderLink = "Link"

  private val nonceQueue: BlockingQueue[String] = new LinkedBlockingQueue[String]()

  /** blocks until a nonce value is available */
  def takeNonce(): String = {
    val nonce = nonceQueue.poll( 0, TimeUnit.SECONDS )
    if( nonce != null ) {
      nonce
    }
    else {
      acmeServer.future.foreach { server =>
        httpGetNonce( server.dir )
      }
      nonceQueue.take
    }
  }

  /** insert nonce into queue if we gone one */
  private def putNonce( opt: Option[String]): Unit = {
    opt.foreach( nonce => nonceQueue.put( nonce ) )
  }

  /** low level method */
  private def httpGET(uri: String, headers: Map[String, String] = Map.empty ): Future[Response] = {
    logger.info( "GET {}", uri )

    try {
      wsClient.url( uri ).withHeaders( headers.toList : _* ).get().map { response =>
        val statusText: String = response.statusText
        logger.debug( "GET {} receives response {}", uri, statusText )

        val r = new Response( response )
        putNonce( r.nonce )

        r
      }
    }
    catch {
      case e: Exception =>
        logger.error( "Failed GET {}", uri, e )
        Future.failed( e )
    }
  }

  /** low level method - only HEAD request to retrieve new Nonce header */
  private def httpGetNonce(uri: String ): Unit = {
    logger.info( "HEAD {}", uri )

    wsClient.url( uri ).head().map { response =>
      logger.debug( "{} Got a Nonce HEAD response {}", uri, response.statusText )
      putNonce( response.header( AcmeProtocol.NonceHeader ) )
    }
  }

  /** low level method */
  private def httpPOST( uri: String, mime: String, bytes: String ): Future[Response] = {
    wsClient.url( uri )
      .withHeaders(
          HttpHeaders.CONTENT_TYPE -> mime )
      .post( bytes ).map { resp =>
      val r = new Response(resp)
      putNonce( r.nonce )
      r
    }
  }

  private def httpPOSTbin( uri: String, accept: String, bytes: String ): Future[BResponse] = {
    wsClient.url( uri ) // .withHeaders("content-type" -> "fake/contenttype; charset=utf-8") .contentType(
      .withHeaders(
          HttpHeaders.CONTENT_TYPE -> MimeUrlencoded,
          HttpHeaders.ACCEPT -> accept )
      .post( bytes ).map { resp =>
        val r = new BResponse(resp)
        putNonce( r.nonce )
        r
      }
  }

  def getDirectory(endpoint: String ): Future[AcmeProtocol.Directory] = {
    httpGET( endpoint ).map {
      case Response( 200, body, headers, nonce ) =>
        logger.info( "body= {}, nonce= {}", body, nonce )

        AcmeJson.parseDirectory( body )
      case Response( status, body, headers, nonce ) =>
        throw new IllegalStateException("Unable to get directory index: " + status + ": " + body)
    }
  }

  /** server asks us to accept ToS as response */
  def registration( uri: String, message: String  ): Future[AcmeProtocol.SimpleRegistrationResponse] = {
    httpPOST( uri, MimeUrlencoded, message ).flatMap {
        case Response( 201, body, headers, nonce ) =>
          logger.info("Successfully registered account: {} {} {} {}", uri, body, headers, nonce)
          termsOfService( headers )

        case Response( 400, body, headers, nonce ) if body contains "urn:acme:error:badNonce" =>
          logger.debug("[{}] Expired nonce used, getting new one", uri)
//          getNonce(client).flatMap { gotNonce =>
//            registration(client, numTry) // we don't count this as an error
//          }
          Future.failed( new IllegalStateException("400 nonce expired" ) )

        case Response( 409, body, headers, nonce ) =>
          logger.info("[{}] We already have an account", uri)
          termsOfService( headers )

        case Response( status, body, headers, nonce ) =>
          logger.error("[{}] Unable to register account after error {} tried {}", uri, status.toString, body )
          throw new IllegalStateException("Unable to register: " + status + ": " + body)
    }
  }

  private def termsOfService( headers: Map[String, Seq[String]] ): Future[AcmeProtocol.SimpleRegistrationResponse] = {
      val regUrl = headers(HttpHeaders.LOCATION).head
      logger.info("  . follow up: {}", regUrl )
      val termsUrl = findTerms( headers )
      logger.info("  . terms of service: {}", termsUrl )
      Future.successful( AcmeProtocol.SimpleRegistrationResponse( regUrl, termsUrl ) )
  }

  /** server provides list of challenges as response */
  def authorize( uri: String, message: String  ): Future[AcmeProtocol.AuthorizationResponse] = {
    httpPOST( uri, MimeUrlencoded, message ).flatMap {
        case Response( 201, body, headers, nonce ) =>
          logger.info("Successfully authorized account: {} {} {} {}", uri, body, headers, nonce)
          Future.successful( AcmeJson.parseAuthorization( body ) )
        case Response( status, body, headers, nonce ) =>
          logger.error("[{}] Unable to authorized account after error {} tried {}", uri, status.toString, body )
          throw new IllegalStateException("Unable to authorize: " + status + ": " + body)
    }
  }

  /** accept ToS to complete the registration: we need to pass the detected "terms of service" doc url to indicate agreement
   *  @param uri regURL returned as Location by new-reg call, for example "https://acme-staging.api.letsencrypt.org/acme/reg/930540"
   *  @param message registration message wrapped as JWS
   */
  def agreement( uri: String, message: String ): Future[AcmeProtocol.RegistrationResponse] = {
    logger.info("[{}] Handling agreement", uri)
      httpPOST( uri, MimeUrlencoded, message ).flatMap {
        case Response(code, body, headers, nonce) if code < 250 =>
          logger.info("[{}] Successfully signed Terms of Service: {} {} {} {}", uri, body, headers, nonce)
          Future.successful( AcmeProtocol.RegistrationResponse() )

        case Response(400, body, headers, nonce) if body contains "urn:acme:error:badNonce" =>
          logger.error("[{}] Expired nonce used, getting new one: {} {} {} {}", uri, body, headers, nonce)
          throw new IllegalStateException("Unable to sign agreement: error 400: " + body)

        case Response(status, body, headers, nonce) =>
          logger.error("[{}] Unable to sign Terms of Service: {} {} {} {}", uri, body, headers, nonce)
          throw new IllegalStateException("Unable to sign agreement: " + status + ": " + body)
      }
  }

  /** locate the terms-of-service link and get the URI */
  private def findTerms(headers: Map[String, Seq[String]]): Option[String] = {
    val linksEntry = headers.get(HeaderLink)
    linksEntry match {
      case None => None
      case Some( links ) =>
        links.foreach{ item => logger.info( "Link: {}", item ) }

        links.find(_.endsWith(";rel=\"terms-of-service\""))
          .flatMap(_.split(">").headOption.map(_.replaceAll("^<", "")))
    }
  }


  /** indicate that we would like to try the particular challenge */
  def challenge( uri: String, message: String ): Future[AcmeProtocol.ChallengeType] = {
    logger.debug("[{}] Accepting challenge", uri )

    httpPOST( uri, MimeUrlencoded, message ).flatMap {
      case Response(status, body, headers, nonce) if status < 250 =>
        logger.info("[{}] Successfully accepted challenge: {} {} {} {}", uri, body, headers, nonce)
        Future.successful( AcmeJson.parseHttpChallenge( body ) )

      case Response(status, body, headers, nonce) =>
        logger.error("[{}] Unable to accept challenge: {} {} {} {}", uri, body, headers, nonce)
        throw new IllegalStateException("Unable to accept challenge: " + status + ": " + body)
    }
  }

  def challengeDetails(uri: String): Future[AcmeProtocol.ChallengeType] = {
    httpGET( uri ).map {
      case Response(status, body, headers, nonce) if status < 250 =>
        logger.info( "status= {} body= {}, nonce= {}", status.toString, body, nonce )

        AcmeJson.parseChallenge( body )
      case Response(status, body, headers, nonce) =>
        throw new IllegalStateException("Unable to get challenge status details: " + status + ": " + body)
    }
  }

  /** request a certificate */
  def issue( uri: String, message: String ): Future[X509Certificate] = {
    logger.debug("[{}] Requesting certificate", uri )

    httpPOSTbin( uri, AcceptMimeCert, message ).flatMap {
      case BResponse(status, body, headers, nonce) if status < 250 =>
        logger.info("[{}] Successfully requested certificate: {} {} {}", uri, status.toString, headers, nonce)
        Future.successful( KeyStorageUtil.parseCertificate( body.toByteBuffer.array() ) )

      case BResponse(status, body, headers, nonce) =>
        logger.error("[{}] Unable to request certificate: {} {} {} {}", uri, body, headers, nonce)
        throw new IllegalStateException("Unable to request certificate: " + status + ": " + body)
    }
  }


}
