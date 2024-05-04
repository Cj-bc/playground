{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Text.Megaparsec
import Data.ByteString as BS

main :: IO ()
main = do
  f <- BS.readFile "resources/preambleonly.wasm"
  print $ parse wasmModule "" f
