{-# Language DeriveGeneric, DeriveFunctor, DeriveDataTypeable #-}
module Language.Skompute.Syntax where

import Data.Data
import GHC.Generics
import Data.Dynamic
import Language.Haskell.TH hiding (Foreign)
import Language.Haskell.TH.Syntax hiding (Foreign)

data ForeignArg = HaskellArg | SkArg
  deriving (Eq, Ord, Show, Generic)

data Foreign = Foreign { _value :: Dynamic, _args :: [ForeignArg]
                       , _retForeign :: Bool}
  deriving (Show, Generic)

data SkomputeSyn a = S | K | I | B | C | SkomputeSyn a :- SkomputeSyn a
                | F a
  deriving (Show, Generic, Functor, Data)

type Skompute = SkomputeSyn Foreign

hnf :: Skompute -> Skompute
hnf x = case hnf' x of
  (x', True)  -> hnf x'
  (x', False) -> x'

-- Perform a head normal reduction, returning whether or not
-- a reduction has occurred
hnf' :: Skompute -> (Skompute, Bool)
hnf' (I :- x) = (x, True)
hnf' (K :- x :- _) = (x, True)
hnf' (S :- x :- y :- z) = ((x :- z) :- (y :- z), True)
hnf' (C :- x :- y :- z) = (x :- z :- y, True)
hnf' (B :- x :- y :- z) = (x :- (y :- z), True)
hnf' (F (Foreign v [] False)) = (fromDyn v (error "Not Skomputer code"), True)
hnf' (f@(F (Foreign _ [] False)) :- y) = (hnf f :- y, True)
hnf' (F (Foreign f (HaskellArg:args) b) :- F (Foreign x [] True)) = (F $ Foreign (dynApp f x) args b , True)
hnf' (F (Foreign f (SkArg:args) b) :- y) = (F (Foreign (dynApp f (toDyn y)) args b) , True)
hnf' (f@(F (Foreign _ (HaskellArg:_) _)) :- y) = case hnf' y of
  (y', b) -> (f :- y', b)
hnf' (x :- y) = case hnf' x of
  (x', b) -> (x' :- y, b)
hnf' x = (x, False)

nf :: Skompute -> Skompute
nf x = case hnf x of
  y :- z -> nf y :- nf z
  z -> z

run :: (Typeable a) => Skompute -> a
run f = case nf f of
  (F (Foreign x [] True)) -> case fromDynamic x of
    Just a -> a
    Nothing -> error ("run: " ++ show x ++ " is not the correct type")
  res -> error $ "run: Not a Haskell value " ++ (show res)

link :: SkomputeSyn String -> Q Exp
link unlinked = do
  (fName, appName) <- do
    fNameLookup <- lookupValueName "Language.Skompute.Syntax.F"
    case fNameLookup of
      Just fName -> do
        Just appName <- lookupValueName "Language.Skompute.Syntax.:-"
        return (fName, appName)
      Nothing -> do
        Just fName <- lookupValueName "Language.Skompute.F"
        Just appName <- lookupValueName "Language.Skompute.:-"
        return (fName, appName)

  dat <- liftData unlinked
  go dat appName fName
  where
    go (AppE (AppE (ConE app) x) y) appName fName | app == appName = do
      x' <- go x appName fName
      y' <- go y appName fName
      return (AppE (AppE (ConE app) x') y')
    go (AppE (ConE f) str) _ fName | f == fName = do
      Just s' <- lookupValueName (makeString str)
      return (AppE (ConE f) (VarE s'))
    go x _ _ = return x
    makeString (ConE _) = []
    makeString (AppE (AppE _ (LitE (CharL x))) xs) = x : makeString xs
    makeString _ = error "Not a string"
