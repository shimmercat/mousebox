{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell, DeriveGeneric #-}
module MouseBox.LeafCertificate(
                    makeLeafCertificate
                  , makeCertificateSigningRequest
                  , DomainList
       ) where


import qualified    Data.ByteString                                    as B
-- import qualified    Data.ByteString.Lazy                               as LB
-- import qualified    Data.Binary                                        as Bn
import              Data.ByteString.Char8                              (
                                                                        --pack,
                                                                        unpack
                                                                       )
--import              Data.Aeson                                         (decode, encode)

import "crypto-api" Crypto.Random
import qualified "cryptonite" Crypto.PubKey.RSA.Types                  as CT
import "cryptonite" Crypto.Hash.Algorithms                             (SHA512(..))
--import qualified    Data.ByteString                                     as B
--import qualified    Data.ByteString.Lazy                                as LB
import qualified    Data.Text                                           as Tx
--import              Data.Text.IDN.IDNA                                  (toASCII, defaultFlags)

import              Codec.Crypto.RSA.Pure
import qualified    Codec.Crypto.RSA.Pure                               as Pu
import              Data.X509
import              Data.ASN1.OID
import              Data.ASN1.Types                                     (ASN1Object(..)  )
--import              Data.ASN1.Types.String
import              Data.ASN1.Encoding                                  (encodeASN1')
import              Data.ASN1.BinaryEncoding                            (DER(..))
import              Data.Hourglass.Types
import              Crypto.PubKey.RSA.Types
import              Data.PEM
--import qualified    Data.Binary                                         as Bn
--import              GHC.Generics                                        (Generic)

import qualified    Control.Lens                                        as L
import              Control.Lens                                        ( (^.) )

import              Data.X509.PKCS10

import              MouseBox.CertificationAuthority
import              MouseBox.Environment
import              MouseBox.PKCS8
import              MouseBox.Utils





type DomainList = [Tx.Text]


type MyKeyPair = (Pu.PublicKey, Pu.PrivateKey)

-- | Creates a signing request to be used by external parties to make this a "legit"
--   certificate. Notice that this signing request is not used by Mousebox itself, which
--   just creates a certificate out of thin air (and the private key).
--
--  This function returns the csr in pem format
makeCertificateSigningRequest :: MyKeyPair -> SerializedEnvironment -> DomainList -> IO PEM
makeCertificateSigningRequest (public_key, private_key) ser_env domains =
  let
    Codec.Crypto.RSA.Pure.PublicKey public_size' public_n' public_e' =  public_key
    Codec.Crypto.RSA.Pure.PrivateKey
        private_pub
        private_d
        private_p
        private_q
        private_dP
        private_dQ
        private_qinv = private_key

    pubkey_to_use = CT.PublicKey public_size' public_n' public_e'

    privkey_to_use = CT.PrivateKey
        pubkey_to_use
        private_d
        private_p
        private_q
        private_dP
        private_dQ
        private_qinv

    alt_dn = ExtSubjectAltName $ map (AltNameDNS . unpack . internetDomainText2ByteString) domains

    ext_attrs = PKCS9Attributes [
           PKCS9Attribute alt_dn,
           -- PKCS9Attribute (ExtSubjectKeyId . publicKeyHasher $ public_key ),
           PKCS9Attribute $ ExtBasicConstraints False Nothing,
           PKCS9Attribute $ ExtKeyUsage [KeyUsage_digitalSignature,KeyUsage_nonRepudiation,KeyUsage_keyEncipherment]
            ]

    subject_attrs = makeX520Attributes [
            (X520CommonName, Tx.unpack . head $ domains),
            (X520OrganizationName, ser_env ^. businessName_SE),
            (X520CountryName, ser_env ^. country_SE)
            ]

--     certificate_request_info = CertificationRequestInfo {
--         -- 0 here means "1"
--         version = Version 0,
-- ,
--         subjectPublicKeyInfo = pubkey_to_use,

--         }

   in do
      Right req <-
        generateCSR
            subject_attrs
            ext_attrs
            (KeyPairRSA pubkey_to_use  privkey_to_use)
            SHA512

      return $ toPEM req



-- | Returns a binary representation of the new registry,  a DER representation of the certificate and a DER
-- representation of the private key of the newly created certificate and the private key itself.
makeLeafCertificate :: PersistentCARegistry
  -> DomainList
  ->  IO (PersistentCARegistry, B.ByteString, B.ByteString, B.ByteString, MyKeyPair)
makeLeafCertificate ca_registry domains = do
    g <- newGenIO :: IO SystemRandom
    let
        -- These are the keys of the subject certificate.
        Right (public_key, private_key''', _) = generateKeyPair g 2048
        private_key = completePrivateKey private_key'''

    let
        issuer_public_key     = ca_registry ^. caPubKey_PCAR
        issuer_private_key    = ca_registry ^. caPrivKey_PCAR
        issuer_common_name    = ca_registry ^. issuerCommonName_PCAR
        current_serial_number = ca_registry ^. currentSerialNumber_PCAR
        new_ca_registry       = L.over currentSerialNumber_PCAR ( + 1 ) ca_registry

        --Codec.Crypto.RSA.Pure.PublicKey i_public_size' i_public_n' i_public_e' =  issuer_public_key
        Codec.Crypto.RSA.Pure.PublicKey public_size' public_n' public_e' =  public_key
        pubkey_to_use = PubKeyRSA $ Crypto.PubKey.RSA.Types.PublicKey public_size' public_n' public_e'

        -- All identitiy information will go somewhere else
        subject_dn = DistinguishedName [
          (getObjectID DnCommonName, stringize . internetDomainText2ByteString . head $ domains)
              ]

        alt_dn = ExtSubjectAltName $ map (AltNameDNS . unpack . internetDomainText2ByteString) domains

        ca_dn = DistinguishedName [
          (getObjectID DnCommonName, stringize issuer_common_name),
          (getObjectID DnCountry, stringize "SE"),
          (getObjectID DnOrganization, stringize "MouseBox by Zunzun AB"),
          (getObjectID DnOrganizationUnit, stringize "Testing")
          ]

        a_cert = Certificate {
            certVersion = 2 -- There are three different versions of X.509.
            -- The certificate has to claim what version it uses.
            -- So you'll find this Version: field in the certificate.
            -- The most recent version is 3 and this is the most used version.
            -- X.509 version 3 defines extensions.
            -- The version is encoded with base 0, so version 3 is denoted as 3-1 =
            -- 2
            -- So for example, they are used for certificate chains.
           , certSerial = fromIntegral current_serial_number
           , certSignatureAlg = SignatureALG HashSHA256 (pubkeyToAlg pubkey_to_use)
           , certIssuerDN = ca_dn
           , certValidity = (
               DateTime {dtDate = Date 2015 January 1, dtTime = TimeOfDay 0 0 0 0},
               DateTime {dtDate = Date 2025 December 31, dtTime = TimeOfDay 23 59 0 0}
               )
           , certSubjectDN = subject_dn
           , certPubKey = pubkey_to_use
           -- TODO: We need to set a few extensions here....
           , certExtensions = Extensions (Just
               -- The set of extensions below is specific to CAs
               [
                   extensionEncode True  {-Critical-} (ExtKeyUsage [KeyUsage_digitalSignature, KeyUsage_keyEncipherment]),
                   extensionEncode True               alt_dn,
                   extensionEncode False              (ExtSubjectKeyId . publicKeyHasher $ public_key ),
                   extensionEncode False              (ExtAuthorityKeyId . publicKeyHasher $ issuer_public_key )
               ]
             )
          }

        (signed_exact_obj, _) = objectToSignedExact (hereSign issuer_private_key) a_cert
        pem_formatted = PEM {
          pemName = "CERTIFICATE",
          pemHeader = [],
          pemContent = encodeSignedObject signed_exact_obj
          }
        pem_encoded= pemWriteBS pem_formatted
        privkey_pem_formatted = PEM {
          pemName = "RSA PRIVATE KEY",
          pemHeader = [],
          pemContent = encodeASN1' DER $ toASN1 private_key []
          }
        privkey_pkcs8_formatted = PEM {
          pemName = "PRIVATE KEY",
          pemHeader = [],
          pemContent = encodeASN1' DER $ encodePKCS8 (PKCS8Encoding private_key) []
          }
        privkey_pem_encoded = pemWriteBS privkey_pem_formatted
        privkey_pkcs8_pem_encoded = pemWriteBS privkey_pkcs8_formatted

    --putStrLn . show $ private_key
    return (new_ca_registry, pem_encoded, privkey_pem_encoded, privkey_pkcs8_pem_encoded,(public_key, private_key))
