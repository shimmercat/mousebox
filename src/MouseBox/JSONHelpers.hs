{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module MouseBox.JSONHelpers
       (
           extractPropertyName
       )
       where


import qualified                           Data.ByteString                   as B
import                                     Text.Regex.TDFA.ByteString
import                                     Text.Regex.Base.RegexLike
import                                     Data.Array


extractPropertyName :: B.ByteString -> B.ByteString
extractPropertyName prop_name =
    case m of
        Right (Just arr)  -> subset $ arr ! 1

        Left _ -> prop_name
  where
    rg = compile defaultCompOpt defaultExecOpt "_([a-zA-Z0-9]+)_([A-Z]+)"
    g  = case rg of
       Left err -> error err
       Right x -> x
    m = execute g prop_name
    subset (start,len) = B.take len . B.drop start $ prop_name
