{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell, DeriveGeneric #-}
module MouseBox.LeafCertificate(
                    makeLeafCertificate
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
--import qualified    Data.ByteString                                     as B
--import qualified    Data.ByteString.Lazy                                as LB

import              Codec.Crypto.RSA.Pure
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

import              MouseBox.CertificationAuthority
import              MouseBox.PKCS8
import              MouseBox.Utils



type DomainList = [B.ByteString]


-- Retuirns a binary representation of the new registry,  a DER representation of the certificate and a DER
-- representation of the private key of the newly created certificate.
makeLeafCertificate :: PersistentCARegistry -> DomainList ->  IO (PersistentCARegistry, B.ByteString, B.ByteString, B.ByteString)
makeLeafCertificate ca_registry domains = do
    g <- newGenIO :: IO SystemRandom
    let
        -- These are the keys of the subject certificate.
        Right (public_key, private_key, _) = generateKeyPair g 2048

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
              ]

        alt_dn = ExtSubjectAltName $ map (AltNameDNS . unpack) domains

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
            -- So for example, they are used for certificate chains.
           , certSerial = fromIntegral current_serial_number
           , certSignatureAlg = SignatureALG HashSHA384 (pubkeyToAlg pubkey_to_use)
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
                   extensionEncode True  {-Critical-} (ExtKeyUsage [KeyUsage_digitalSignature, KeyUsage_keyEncipherment, KeyUsage_keyAgreement]),
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

    return (new_ca_registry, pem_encoded, privkey_pem_encoded, privkey_pkcs8_pem_encoded)
