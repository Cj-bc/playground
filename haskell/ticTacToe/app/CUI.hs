module Main where

import Brick

import TicTacToe
import UI.CUI

main = defaultMain app (initializeBoard, O) >>= \s -> putStrLn $ show s
