{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module MouseBox.Mouseboxf(
                 MouseboxfData(..)
               , domains_Mf

               , InternetDomain
               , internetDomainText2ByteString
       )
       where


--import           Control.Monad                                         (when, unless)
import           Control.Lens                                          hiding ( (.=) )
import           Control.Exception                                     (throw)

import qualified Data.ByteString                                       as B
--import qualified Data.ByteString.Lazy                                  as LB
--import qualified Data.Binary                                           as Bn
--import           Data.ByteString.Char8                                 (pack, unpack)
import           Data.Yaml                                             (FromJSON(..), (.:), Value(..), object,
                                                                        ToJSON(..), (.=)
                                                                       )
--import           Data.Aeson.TH

import qualified Data.Text                                             as Tx
import           Data.Text.IDN.IDNA                                    (toASCII, defaultFlags)


import           MouseBox.Exceptions                                   (BadDomainNameException(..))


-- Domains are given in plain Text
type InternetDomain = Tx.Text


data MouseboxfData = MouseboxfData {
    _domains_Mf :: [InternetDomain]
    } deriving Show


makeLenses ''MouseboxfData


instance FromJSON MouseboxfData where

    parseJSON (Object v) = MouseboxfData <$>
                           v .: "domains"
    parseJSON _          = mempty


instance ToJSON MouseboxfData where

    toJSON (MouseboxfData domains) =
        object [ "domains" .= domains ]


internetDomainText2ByteString :: Tx.Text -> B.ByteString
internetDomainText2ByteString txt = let
    either_result = toASCII defaultFlags txt
  in case either_result  of
    Left  _     -> throw $ BadDomainNameException txt
    Right bs    -> bs
