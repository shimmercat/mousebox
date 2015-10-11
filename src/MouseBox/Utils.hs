{-# LANGUAGE PackageImports #-}
module MouseBox.Utils
       (
         shortRandomName
       , stringize
       , publicKeyHasher
       , hereSign
       ) where


import qualified Data.ByteString                                       as B
import qualified Data.ByteString.Lazy                                  as LB
--import           Data.ByteString.Char8                                 (pack, unpack)


import qualified "crypto-api" Crypto.Random                            as CR
import           Control.Monad.CryptoRandom
import           Data.ASN1.Types.String
import           Data.X509
import           Codec.Crypto.RSA.Pure
import qualified Data.Binary                                           as Bn


shortRandomName :: Int -> IO B.ByteString
shortRandomName name_length = do
    g <- CR.newGenIO :: IO CR.SystemRandom
    let
        bytes = take name_length $ crandomRs (97,122) g
        bs = B.pack bytes
    return bs


stringize  :: B.ByteString -> ASN1CharacterString
stringize s = ASN1CharacterString IA5 s


publicKeyHasher :: Codec.Crypto.RSA.Pure.PublicKey -> B.ByteString
publicKeyHasher pk = LB.toStrict . LB.take 20 . Bn.encode $ pk


hereSign :: Codec.Crypto.RSA.Pure.PrivateKey -> B.ByteString -> (B.ByteString, SignatureALG, ())
hereSign _private_key stuff_to_sign = let
    Right  bs_sign = rsassa_pkcs1_v1_5_sign hashSHA384 _private_key . LB.fromStrict $ stuff_to_sign
  in (LB.toStrict bs_sign, SignatureALG HashSHA384 PubKeyALG_RSA, () )
