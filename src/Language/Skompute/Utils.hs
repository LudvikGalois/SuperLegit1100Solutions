{-# Language QuasiQuotes #-}
module Language.Skompute.Utils where

import Data.Dynamic
import Language.Skompute.Compiler (λ)
import Language.Skompute.Syntax

-- An unintelligent embedding
foreignDat :: (Typeable a) => a -> Foreign
foreignDat x = Foreign (toDyn x) [] True

-- A slightly more intelligent embedding which knows about
-- lists and pairs
class ForeignData a where
  foreignData :: a -> Foreign

instance ForeignData Integer where
  foreignData = foreignDat
  
instance ForeignData Int where
  foreignData = foreignDat

instance ForeignData Double where
  foreignData = foreignDat

instance ForeignData Bool where
  foreignData True = Foreign (toDyn ([λ|λxy.x|] :: Skompute)) [] False
  foreignData False = Foreign (toDyn ([λ|λxy.y|] :: Skompute)) [] False

instance (ForeignData a, ForeignData b) => ForeignData (a,b) where
  foreignData (x,y) = Foreign (toDyn [λ| λp. p `hx `hy|]) [] False
    where hx = foreignData x
          hy = foreignData y 
          
instance (ForeignData a) => ForeignData [a] where
  foreignData xs = Foreign (toDyn xs') [] False
    where xs' = foldr f [λ|#nil|] xs
          f x acc = [λ|#cons `a|] :- acc
            where a = foreignData x

instance (ForeignData a) => ForeignData (Maybe a) where
  foreignData Nothing = Foreign (toDyn ([λ|λxy.x|] :: Skompute)) [] False
  foreignData (Just x) = Foreign (toDyn [λ|λxy.y `a|]) [] False
    where a = foreignData x

dZero :: Foreign
dZero = foreignData (0 :: Double)

dOne :: Foreign
dOne = foreignData (1 :: Double)

dTwo :: Foreign
dTwo = foreignData (2 :: Double)

dBinOp :: (Double -> Double -> Double) -> Foreign
dBinOp f = Foreign (toDyn f) [HaskellArg, HaskellArg] True

dCmp :: (Double -> Double -> Bool) -> Foreign
dCmp f = Foreign (toDyn g) [HaskellArg, HaskellArg] False
  where
    g :: Double -> Double -> Skompute
    g x y | f x y     = [λ|λxy.x|]
          | otherwise = [λ|λxy.y|]

dDiv :: Foreign
dDiv = dBinOp (/)

dMult :: Foreign
dMult = dBinOp (*)

dAdd :: Foreign
dAdd = dBinOp (+)

dSub :: Foreign
dSub = dBinOp (-)

dLt :: Foreign
dLt = dCmp (<)

dLe :: Foreign
dLe = dCmp (<=)

iZero :: Foreign
iZero = foreignData (0 :: Int)

iOne :: Foreign
iOne = foreignData (1 :: Int)

iTwo :: Foreign
iTwo = foreignData (2 :: Int)

iBinOp :: (Int -> Int -> Int) -> Foreign
iBinOp f = Foreign (toDyn f) [HaskellArg, HaskellArg] True

iCmp :: (Int -> Int -> Bool) -> Foreign
iCmp f = Foreign (toDyn g) [HaskellArg, HaskellArg] False
  where
    g :: Int -> Int -> Skompute
    g x y | f x y     = [λ|λxy.x|]
          | otherwise = [λ|λxy.y|]

iDiv :: Foreign
iDiv = iBinOp div

iMult :: Foreign
iMult = iBinOp (*)

iAdd :: Foreign
iAdd = iBinOp (+)

iSub :: Foreign
iSub = iBinOp (-)

iLt :: Foreign
iLt = iCmp (<)

iLe :: Foreign
iLe = iCmp (<=)
