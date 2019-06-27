{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib
import DiceAndNukes

generateSchema diceAndNukes
--generateSchema emptySchema

query1 :: Query
query1 = Query { rollDice = [5], launchNukes = 3 }

main :: IO ()
main = print "works"
