{-# Language QuasiQuotes #-}

module LambdaPrelude where

import Language.LambdaCalc.Syntax (Lambda)
import Language.LambdaCalc.Parser (λlib)
import Data.Map (Map)

stdlib :: Map String Lambda
stdlib = [λlib|
0 ::= [λsz.z]
1 ::= [λsz.sz]
succ ::= [λnsz.s(nsz)]
plus ::= [λnmsz.ns(msz)]
mult ::= [λnmsz.n(ms)z]
T ::= [λxy.x]
F ::= [λxy.y]
Y ::= [λf.(λx.f(xx))(λx.f(xx))]
P ::= [λxyf.fxy]
fst ::= [λp.p #T]
snd ::= [λp.p #F]
nil ::= [#0]
cons ::= [λxyfz.fx(yfz)]
|]

