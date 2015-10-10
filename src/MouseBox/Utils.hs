{-# LANGUAGE PackageImports #-}
module MouseBox.Utils
       (
         shortRandomName
       ) where


import qualified Data.ByteString                                       as B
-- import qualified Data.ByteString.Lazy                                  as LB
--import           Data.ByteString.Char8                                 (pack, unpack)


import qualified "crypto-api" Crypto.Random                            as CR
import           Control.Monad.CryptoRandom


shortRandomName :: Int -> IO B.ByteString
shortRandomName name_length = do
    g <- CR.newGenIO :: IO CR.SystemRandom
    let
        bytes = take name_length $ crandomRs (97,122) g
        bs = B.pack bytes
    return bs
