{-# LANGUAGE PackageImports, RecordWildCards #-}
module MouseBox.Utils
       (
         shortRandomName
       , stringize
       , publicKeyHasher
       , hereSign
       , recursivelyCreateDirectory
       , internetDomainText2ByteString
       , completePrivateKey
       ) where

import           Control.Monad                                         (when {-, unless-})
import           Control.Exception

import qualified System.PosixCompat.Files                              as SPF
import           System.Directory
import           System.FilePath

import qualified Data.Text                                             as Tx
import           Data.Primitive.PunyCode                               (toASCII)

import qualified Data.ByteString                                       as B
import qualified Data.ByteString.Lazy                                  as LB
import           Data.ByteString.Char8                                 (pack, unpack)


import qualified "crypto-api" Crypto.Random                            as CR
import           Control.Monad.CryptoRandom
import           Data.ASN1.Types.String
import           Data.X509
import           Codec.Crypto.RSA.Pure
import qualified Codec.Crypto.RSA.Pure                                 as P
import qualified Data.Binary                                           as Bn

import           MouseBox.Exceptions



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
    Right  bs_sign = rsassa_pkcs1_v1_5_sign hashSHA256 _private_key . LB.fromStrict $ stuff_to_sign
  in (LB.toStrict bs_sign, SignatureALG HashSHA256 PubKeyALG_RSA, () )


completePrivateKey :: Codec.Crypto.RSA.Pure.PrivateKey -> Codec.Crypto.RSA.Pure.PrivateKey
completePrivateKey p@P.PrivateKey {..} = p {
    private_dP = private_d `mod` (private_p - 1)
  , private_dQ = private_d `mod` (private_q - 1)
  , private_qinv = P.modular_inverse private_q private_p
    }


recursivelyCreateDirectory :: B.ByteString ->  IO ()
recursivelyCreateDirectory pth  = do
    directory_dont_exist <- SPF.fileExist $ unpack  pth
    when (not directory_dont_exist) $ do
        --putStrLn . show $ (upper_directory, last_path_component)
        recursivelyCreateDirectory $ pack  upper_directory
        --putStrLn $ show last_path_component
        createDirectory (upper_directory </> last_path_component)
  where
    (upper_directory, last_path_component) = splitFileName . dropTrailingPathSeparator . unpack $ pth


internetDomainText2ByteString :: Tx.Text -> B.ByteString
internetDomainText2ByteString txt = let
    maybe_result = toASCII  txt
  in case maybe_result  of
    Nothing     -> throw $ BadDomainNameException txt
    Just bs    -> bs
