{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module DiceAndNukes where

import Data.Attoparsec.Text

import Data.GraphQL.AST
import Data.GraphQL.Parser

schema = "type Query {rollDice(num: Int!, sides: Int!): [Int], launchNukes: Int}"

query = " { rollDice(3, 6), launchNukes }"

Right diceAndNukes = parseOnly document schema

emptySchema = Document []
