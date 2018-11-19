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

import java.security.KeyStore

import com.typesafe.scalalogging.Logger
import java.security.cert.X509Certificate

import org.bouncycastle.pkcs.PKCS10CertificationRequest

import com.nimbusds.jose.jwk.RSAKey

object KeyStorage {
  /** we like to hardcode some sensible defaults - but allow you to override if wanted */
  case class Params( DomainCertAlias: String, ChainCertAlias: String, KeystorePassword: String, UserKeystore: String, AppKeystore: String, UserKey: String )

  val DefaultPassword: String = "changeit"

  val Defaults = Params( DomainCertAlias = "domain",
      ChainCertAlias = "root",
      KeystorePassword = getPropertyOrDefault( "play.server.https.keyStore.password", DefaultPassword ),
      UserKeystore = getPropertyOrDefault( "play.user.keyStore.path", "conf/private.keystore" ),
      AppKeystore = getPropertyOrDefault( "play.server.https.keyStore.path", "conf/domain.keystore" ),
      UserKey = "user.key" )

    def getPropertyOrDefault( propertyName: String, defaultValue: String ): String = {
      val prop = Option( System.getProperty( propertyName ) )
      prop match {
        case None => defaultValue
        case Some(propValue) => propValue
      }
    }

}

class KeyStorage( params: KeyStorage.Params ) {
    private val logger = Logger[KeyStorage]

    lazy val userKeystore: KeyStore = KeyStorageUtil.loadKeystore( params.UserKeystore, params.KeystorePassword )
    lazy val userKey: RSAKey = KeyStorageUtil.getUserKey( params.UserKey, userKeystore, params.UserKeystore, params.KeystorePassword )

    lazy val domainKeystore: KeyStore = KeyStorageUtil.loadKeystore( params.AppKeystore, params.KeystorePassword )
    lazy val domainKey: RSAKey = KeyStorageUtil.getDomainKey( params.DomainCertAlias, domainKeystore, params.AppKeystore, params.KeystorePassword )

  def generateCertificateSigningRequest( domain: String ): PKCS10CertificationRequest = {
      KeyStorageUtil.generateCertificateSigningRequest( domainKey.toKeyPair, domain )
  }

  /** write the newly received domain certificate to the keystore */
  def updateKeyStore(domainCertificate: X509Certificate) = {

    val chain = KeyStorageUtil.getIntermediateChain( domainCertificate )
    if( chain.isPresent ) {
      val rootCert: X509Certificate = chain.get
      KeyStorageUtil.storeCertificateKey( domainKeystore, params.KeystorePassword, domainKey.toKeyPair, params.DomainCertAlias, domainCertificate, rootCert )
      domainKeystore.setCertificateEntry( params.ChainCertAlias, rootCert )
    }
    else {
      KeyStorageUtil.storeCertificateKey( domainKeystore, params.KeystorePassword, domainKey.toKeyPair, params.DomainCertAlias, domainCertificate )
    }

    KeyStorageUtil.saveKeystore( domainKeystore, params.AppKeystore, params.KeystorePassword )
    logger.info("wrote keystore: {}", params.AppKeystore )
  }

  def location: String = params.AppKeystore

  def defaultPassword: Boolean = params.KeystorePassword == KeyStorage.DefaultPassword
}
