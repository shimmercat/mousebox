{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module MouseBox.Mouseboxf(
                 MouseboxfData(..)
               , domains_Mf

               , InternetDomain
               , internetDomainText2ByteString
       )
       where


import           Control.Lens                                          hiding ( (.=) )
import           Control.Exception                                     (throw)

import qualified Data.ByteString                                       as B
import           Data.Yaml                                             (FromJSON(..), (.:), Value(..), object,
                                                                        ToJSON(..), (.=)
                                                                       )

import qualified Data.Text                                             as Tx



import           MouseBox.Exceptions                                   (BadDomainNameException(..))
import           MouseBox.Utils                                        (internetDomainText2ByteString)

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
