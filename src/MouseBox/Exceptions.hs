{-# LANGUAGE DeriveDataTypeable #-}
module MouseBox.Exceptions(
                  BadDomainNameException(..)
       ) where


import            Control.Exception
import qualified  Data.Text                            as Tx
import            Data.Typeable


data BadDomainNameException = BadDomainNameException Tx.Text
    deriving (Show, Typeable)

instance Exception BadDomainNameException
