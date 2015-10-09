{-# LANGUAGE OverloadedStrings #-}
module Main where

import MouseBox.Environment


main :: IO ()
main = do
    ce <- captureEnvironment
    putStrLn . show $ ce
