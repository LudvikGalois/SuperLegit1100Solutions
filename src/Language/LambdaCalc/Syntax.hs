{-# Language DeriveGeneric, DeriveDataTypeable #-}
-- | The Syntax for the λ-calculus.
-- This is super uninteresting

module Language.LambdaCalc.Syntax where

import GHC.Generics
import Data.Data

-- | We'll use `String`s for variables
type Var = String

-- | A λ-expression
data Lambda = Bind String Lambda | Var Var
            | Foreign String | App Lambda Lambda
  deriving (Show, Generic, Data)

isFreeIn :: Var -> Lambda -> Bool
isFreeIn v (Var v') = v /= v'
isFreeIn _ Foreign{} = False
isFreeIn v (App e1 e2) = v `isFreeIn` e1 && v `isFreeIn` e2
isFreeIn v (Bind v' e) = v == v' || v `isFreeIn` e
