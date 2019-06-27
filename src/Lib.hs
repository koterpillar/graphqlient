{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( generateSchema
    ) where

import Data.GraphQL.AST as G
import Data.GraphQL.Parser

import qualified Data.Text as Text

import Control.Monad
import Language.Haskell.TH as H
import Language.Haskell.TH.Syntax as H

generateSchema :: Document -> Q [Dec]
generateSchema (Document defns) = do
  results <- traverse generateTypesForDec defns
  pure $ mconcat results

generateTypesForDec :: Definition -> Q [Dec]
generateTypesForDec (DefinitionType td) = gen2 td
generateTypesForDec _ = pure []

gen2 :: TypeDefinition -> Q [Dec]
gen2 (TypeDefinitionObject (ObjectTypeDefinition name [] fields)) = do
  let n = mkName (Text.unpack name)
  let nR = mkName (Text.unpack name ++ "R")
  let queryFields = map mkField fields
  let con = RecC n $ map snd queryFields
  let conR = RecC n $ map mkFieldR fields
  pure $ [ DataD [] n [] Nothing [con] [] -- query type
         , DataD [] nR [] Nothing [conR] [] -- result type
         ] ++ map fst queryFields

mkField :: FieldDefinition -> (Dec, VarBangType)
mkField (FieldDefinition fn args _) = (fieldDecl, fieldField) where
  fieldDecl = _
  fieldType = _
  fieldField = (mkName (Text.unpack fn)
               , Bang NoSourceUnpackedness NoSourceStrictness
               , referToType fieldType)

mkFieldR :: FieldDefinition -> VarBangType
mkFieldR (FieldDefinition fn _ t) = (mkName (Text.unpack fn), Bang NoSourceUnpackedness NoSourceStrictness, referToType t)

referToType :: G.Type -> H.Type
referToType (TypeNamed (NamedType n)) = ConT $ mkName $ Text.unpack n
referToType (TypeList (ListType t)) = AppT (ConT $ mkName "[]") (referToType t)
