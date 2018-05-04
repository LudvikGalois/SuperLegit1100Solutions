module Language.Skompute.Compiler where

import qualified Language.LambdaCalc.Syntax as L
import qualified Language.LambdaCalc.Parser as P
import qualified Language.Skompute.Syntax as S
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import LambdaPrelude

data Intermed = S | K | I | B | C | Var String | Bind String Intermed
              | Foreign String | App Intermed Intermed

isFreeIn :: String -> Intermed -> Bool
isFreeIn x (Var y) = x == y
isFreeIn x (Bind x' y) | x == x' = False
                       | otherwise = x `isFreeIn` y
isFreeIn x (App e1 e2) = x `isFreeIn` e1 || x `isFreeIn` e2
isFreeIn _ _ = False

compile :: Intermed -> Intermed
compile (App x y) = App (compile x) (compile y)
compile (Bind x b) | not (x `isFreeIn` b) = App K (compile b)
compile (Bind x (Var y)) | x == y = I
compile (Bind x (Bind y e)) | x `isFreeIn` e = compile $ Bind x $ compile (Bind y e)
compile (Bind x (App y z)) | x `isFreeIn` y && x `isFreeIn` z =
                                 App (App S (compile (Bind x y))) (compile (Bind x z))
compile (Bind x (App y z)) | x `isFreeIn` y && not (x `isFreeIn` z) =
                                 App (App C (compile (Bind x y))) (compile z)
compile (Bind x (App y z)) | not (x `isFreeIn` y) && x `isFreeIn` z =
                                 App (App B (compile y)) (compile (Bind x z))
compile x = x

toInterMed :: L.Lambda -> Intermed
toInterMed (L.App x y) = App (toInterMed x) (toInterMed y)
toInterMed (L.Foreign f) = Foreign f
toInterMed (L.Bind x y) = Bind x (toInterMed y)
toInterMed (L.Var x) = Var x

fromInterMed :: Intermed -> S.SkomputeSyn String
fromInterMed (App x y) = (fromInterMed x) S.:- (fromInterMed y)
fromInterMed S = S.S
fromInterMed K = S.K
fromInterMed I = S.I
fromInterMed B = S.B
fromInterMed C = S.C
fromInterMed (Foreign f) = S.F f
fromInterMed (Var _) = error "fromInterMed: Var still in AST"
fromInterMed (Bind _ _) = error "fromInterMed: Bind still in AST"

buildFunction :: String -> Q Exp
buildFunction code = let (Right e) = P.parseProgram stdlib code in
  S.link $ fromInterMed $ compile $ toInterMed e

λ :: QuasiQuoter
λ = QuasiQuoter { quoteExp = buildFunction
                , quotePat = undefined
                , quoteType = undefined
                , quoteDec = undefined}
