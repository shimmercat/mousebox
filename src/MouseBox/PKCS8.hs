
-- TODO: This would give a good standalone module
module MouseBox.PKCS8 (
                    PKCS8Encoding                                     (..)
                  , encodePKCS8
       ) where


import              Codec.Crypto.RSA.Pure
import qualified    Codec.Crypto.RSA.Pure                              as RP
import              Data.X509
import              Data.ASN1.OID
import              Data.ASN1.Types.String
import              Data.ASN1.Types
import              Data.ASN1.Encoding                                 (encodeASN1')
import              Data.ASN1.BinaryEncoding                           (DER(..))
import              Data.Hourglass.Types
import              Crypto.PubKey.RSA.Types
import              Data.PEM
import qualified    Data.Binary                                         as Bn


newtype PKCS8Encoding = PKCS8Encoding RP.PrivateKey


encodePKCS8 :: PKCS8Encoding -> ASN1S
encodePKCS8 (PKCS8Encoding priv_key) prepend_to =
      (Start Sequence)
    :    (IntVal 0)
    :    (Start Sequence)  -- Key Algorithm comes now
    :         (OID   [1,2,840,113549,1,1,1])
    :         Null
    :    (End Sequence)
    :    (OctetString ber_privkey_encoding)
    : (End Sequence)
    : prepend_to
  where
    ber_privkey_encoding = encodeASN1' DER $ toASN1 priv_key []


-- instance ASN1Object PKCS8Encoding where
