{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell, DeriveGeneric #-}
module MouseBox.CertificationAuthority(

      PersistentCARegistry(..)
    , issuerCommonName_PCAR
    , currentSerialNumber_PCAR
    , caPrivKey_PCAR
    , caPubKey_PCAR

    , createCACertificate
    , commonName2DistinguishedName
    , newPersistentCARegistry
                                      ) where

import "crypto-api" Crypto.Random
import qualified    Data.ByteString                                     as B
import qualified    Data.ByteString.Lazy                                as LB

import              Codec.Crypto.RSA.Pure
import              Data.X509
import              Data.ASN1.OID
import              Data.ASN1.Types.String
import              Data.Hourglass.Types
import              Crypto.PubKey.RSA.Types
import              Data.PEM
import qualified    Data.Binary                                         as Bn
import              GHC.Generics                                        (Generic)


import qualified    Control.Lens                                        as L
import              Control.Lens                                        ( (^.), makeLenses )


data PersistentCARegistry = PersistentCARegistry {
      _issuerCommonName_PCAR        :: B.ByteString
    , _currentSerialNumber_PCAR     :: Int
    , _caPrivKey_PCAR               :: Codec.Crypto.RSA.Pure.PrivateKey
    , _caPubKey_PCAR                :: Codec.Crypto.RSA.Pure.PublicKey
    } deriving (Show, Generic)

instance Bn.Binary PersistentCARegistry


makeLenses ''PersistentCARegistry


stringize  :: B.ByteString -> ASN1CharacterString
stringize s = ASN1CharacterString IA5 s


publicKeyHasher :: Codec.Crypto.RSA.Pure.PublicKey -> B.ByteString
publicKeyHasher pk = LB.toStrict . LB.take 20 . Bn.encode $ pk


hereSign :: Codec.Crypto.RSA.Pure.PrivateKey -> B.ByteString -> (B.ByteString, SignatureALG, ())
hereSign _private_key stuff_to_sign = let
    Right  bs_sign = rsassa_pkcs1_v1_5_sign hashSHA384 _private_key . LB.fromStrict $ stuff_to_sign
  in (LB.toStrict bs_sign, SignatureALG HashSHA384 PubKeyALG_RSA, () )


-- A somewhat contrived conversion function
commonName2DistinguishedName :: B.ByteString -> DistinguishedName
commonName2DistinguishedName common_name =  DistinguishedName [
    (getObjectID DnCommonName, stringize common_name),
    (getObjectID DnCountry, stringize "SE"),
    (getObjectID DnOrganization, stringize "MouseBox by Zunzun AB"),
    (getObjectID DnOrganizationUnit, stringize "Testing")
    ]


newPersistentCARegistry ::  B.ByteString -> IO PersistentCARegistry
newPersistentCARegistry disambiguator = do
    g <- newGenIO :: IO SystemRandom
    let
        Right (public_key, private_key, _) = generateKeyPair g 2048
    return $ PersistentCARegistry {
      _issuerCommonName_PCAR = issuer_common_name,
      _currentSerialNumber_PCAR = 10,
      _caPrivKey_PCAR = private_key,
      _caPubKey_PCAR = public_key
      }
  where
    issuer_common_name = "MouseBox Testing Cert. Authority # " `mappend` disambiguator


-- Creates a CA certificate with the given common name, returns it's representation in PEM
-- format.
createCACertificate ::  PersistentCARegistry  ->  B.ByteString
createCACertificate ca_registry  =
  let
    public_key = ca_registry ^. caPubKey_PCAR
    private_key = ca_registry ^. caPrivKey_PCAR
    issuer_common_name = ca_registry ^. issuerCommonName_PCAR
    Codec.Crypto.RSA.Pure.PublicKey public_size' public_n' public_e' =  public_key
    pubkey_to_use = PubKeyRSA $ Crypto.PubKey.RSA.Types.PublicKey public_size' public_n' public_e'
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
       , certSerial = 1
       , certSignatureAlg = SignatureALG HashSHA384 (pubkeyToAlg pubkey_to_use)
       , certIssuerDN = ca_dn
       , certValidity = (
           DateTime {dtDate = Date 2015 January 1, dtTime = TimeOfDay 0 0 0 0},
           DateTime {dtDate = Date 2025 December 31, dtTime = TimeOfDay 23 59 0 0}
           )
       , certSubjectDN = ca_dn
       , certPubKey = pubkey_to_use
       -- TODO: We need to set a few extensions here....
       , certExtensions = Extensions (Just
           -- The set of extensions below is specific to CAs
           [
               extensionEncode True {-Critical-} (ExtKeyUsage [KeyUsage_keyCertSign]),
               extensionEncode True              (ExtBasicConstraints True
                   -- The field below says that leaf certificates must be signed by this one.
                   -- A little bit in the way of extra-security...
                   (Just 0)
               ),
               extensionEncode True               (ExtSubjectKeyId . publicKeyHasher $ public_key )
           ]
         )
      }
    (signed_exact_obj, _) = objectToSignedExact (hereSign private_key) a_cert
    pem_formatted = PEM {
      pemName = "CERTIFICATE",
      pemHeader = [],
      pemContent = encodeSignedObject signed_exact_obj
      }
  in pemWriteBS pem_formatted
