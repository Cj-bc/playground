{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Text.Megaparsec

main :: IO ()
main = do
  print $ parse preamble "" "\0asm"
