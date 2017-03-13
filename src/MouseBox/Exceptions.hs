{-# LANGUAGE DeriveDataTypeable #-}
module MouseBox.Exceptions(
                  BadDomainNameException               (..)
                , MouseBoxWrongRegistryFormat          (..)
       ) where


import            Control.Exception
import qualified  Data.Text                            as Tx
import            Data.Typeable


data BadDomainNameException = BadDomainNameException Tx.Text
    deriving (Show, Typeable)

instance Exception BadDomainNameException


data MouseBoxWrongRegistryFormat = MouseBoxWrongRegistryFormat String
    deriving  (Typeable, Show)


instance Exception MouseBoxWrongRegistryFormat
