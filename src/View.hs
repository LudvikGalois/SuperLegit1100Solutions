{-# LANGUAGE QuasiQuotes #-} -- To inline the λ-calc
--- Copyright 2018 The Australian National University, All rights reserved
module View where

import CodeWorld hiding (Point)
import Data.Dynamic
import Data.List
import Data.Text (pack)
import Language.Skompute
import Model

-- a pixel coordinate is a pair of Int
type Coord = (Int, Int)

-- a pixel value is some shade of grey (0.0 == white, 1.0 = black)
type Shade = Double

-- a raster pixel is represented by a coordinate and a shade
type Pixel = (Coord, Shade)

-- a raster is a list of pixels
type Raster = [Pixel]

coordToPoint :: Resolution -> Coord -> Point
coordToPoint z (x, y) = (x', y')
  where
    x' = fromIntegral x * z
    y' = fromIntegral y * z

pointToCoord :: Resolution -> Point -> Coord
pointToCoord z (x, y) = (x', y')
  where
    x' = round $ x / z
    y' = round $ y / z

-- Update the view based on the model by constructing a rasterised CodeWorld picture
updateView :: Model -> Picture
updateView (Model ss z s t) =
  coordinatePlane &
  pictures (map pixelToPicture $ concatMap (shapeToRaster z s) ss) &
  translated (-13.5) 8 $
  (text . pack) ("Shape: " ++ shapeToText)
  where
    shapeToText = take (length shape - 4) shape
    shape = takeWhile (/= ' ') $ maybe "" show t
    pixelToPicture (c, b) = translated x' y' p
      where
        p = coloured (grey $ 1 - b) $ solidRectangle z z
        (x', y') = coordToPoint z c

-- Construct a raster for a shape at the given resolution (optionally smoothed)
shapeToRaster :: Resolution -> Smooth -> Shape -> Raster
shapeToRaster z s hShape = clean 
                         $ run toRaster -- Core COMP1100 parts
  where
    toRaster = 
      [λ|min ::= [λxy.`iLe xyxy]
         max ::= [λxy.`iLe xyyx]
         map ::= [λfx.x(λab.#cons(fa)b)#nil]
         concatMap ::= [λfx.#map f x (λxy.x #cons y) #nil]
         fromTo ::= [#Y(λrxy.`iLt yx #nil(#cons x(r(`iAdd `iOne x)y)))]
         neg ::= [`iSub `iZero]
         abs ::= [λx.`iLt x `iZero(#neg x)x]
         square ::= [λx.`iMult x x]
         toHList ::= [λx.#map(λy.`pair2 (y `pair1)`dOne)x `cons `nil]
         toHListD ::= [λx.#map(λy.`pair2 ((#fst y) `pair1) (#snd y))x `cons `nil]
         point ::= [λp.#cons p #nil]
         rect ::= [λpq.
           ((λabcd.(λxy.#map (λx.#P xb)x
                 #cons (#map (λx.#P xd)x)
                 #cons (#map (λy.#P ay)y)
                 #cons (#map (λy.#P cy)y))
               (#fromTo ac)(#fromTo(`iAdd `iOne b)(`iSub d `iOne)))
             (#min(#fst p)(#fst q))
             (#min(#snd p)(#snd q))
             (#max(#fst p)(#fst q))
             (#max(#snd p)(#snd q)))]
         bresenham ::= [λpq.
           (λabcd.
             (λef.#Y(λrabcd.`iLt ba #nil
               (#cons (#P ac)(`iLt `iZero d
                 (r(`iAdd `iOne a)b(`iAdd `iOne c)
                   (`iAdd (`iSub d (`iMult `iTwo e)) (`iMult `iTwo f)))
                 (r(`iAdd `iOne a)bc(`iAdd d(`iMult f `iTwo))))))
               acb(`iSub (`iMult `iTwo f)e))
             (`iSub ca) (`iSub db))
           (#fst p)(#snd p)(#fst q)(#snd q)]
         wu ::= [λpq.
           (λabcd.
             (λef.#Y(λrabcd.`iLt ba #nil
               (#cons (#P (#P a (`dFloor c))
                          (`dSub `dOne (`dSub c (`iToD (`dFloor c)))))
               (#cons (#P (#P a (`iAdd `iOne (`dFloor c)))
                          (`dSub c (`iToD (`dFloor c))))
               (r(`iAdd `iOne a)b(`dAdd cd)d))))
               ac(`iToD b)(`dDiv (`iToD f) (`iToD e)))
             (`iSub ca) (`iSub db))
           (#fst p)(#snd p)(#fst q)(#snd q)]
         line ::= [λspq.
           (λab.(λabcd.
             `iLe bd(#P(#P(#P ab)
                          (#P cd))
                       (λx.x))
                     (#P(#P(#P a(#neg b))
                           (#P c(#neg d)))
                       (s(λx.x(λyz.y(λab.#P (#P a(#neg b))z)))
                         (λx.x(λab.#P a(#neg b)))))
                  (λxf.(λabcd.`iLe(#abs(`iSub db))
                                  (#abs(`iSub ac))
                              (#P xf)
                              (#P(#P(#P ba)(#P dc))
                                (s(λx.f(x(λyz.y(λab.#P (#P ba)z))))
                                  (λx.f(x(λab.#P ba))))))
                       (#fst(#fst x))(#snd(#fst x))
                       (#fst(#snd x))(#snd(#snd x)))
                  (λxf.#map f(x (s #wu #bresenham))))
               (#fst a)(#snd a)(#fst b)(#snd b))
             ((`iLe(#fst p)(#fst q)pq))((`iLe(#fst p)(#fst q)qp))
             ]
         poly ::= [λsp.(λx.#fst x
                           (#Y(λrabc.#fst a
                                     ((#line sb(#fst(#snd a)))
                                       #cons
                                       (r(#snd(#snd a))(#fst(#snd a))c))
                                     (#line sbc))
                              (#snd(#snd x))(#fst(#snd x))(#fst(#snd x)))
                           #nil)
                  (p(λxy.#P #T(#P xy))(#P #F #F))]

         bresCircle ::= [#Y(λrxyd.`iLt yx #nil
             (`iLt `iZero d
               (#cons(#P xy)(r(`iAdd x `iOne)
                              (`iSub y `iOne)
                              (`iAdd d(`iAdd(`iMult `iFour (`iSub xy))
                                             `iEighteen))))
               (#cons(#P xy)(r(`iAdd x `iOne)
                              y
                              (`iAdd d(`iAdd(`iMult `iFour x)`iTen))))))]
         bresCircleSmooth ::= [#Y(λrxyde.`iLt yx #nil
             ((λz.
               (`dLt z `dZero 
                 (λn. (#cons (#P (#P xy) (`dAdd `dOne z))
                      (#cons (#P (#P x (`iAdd y `iOne)) (`dSub `dZero z)) n)))
                 (λn. (#cons (#P (#P xy) (`dSub `dOne z))
                      (#cons (#P (#P x (`iSub y `iOne)) z) n))))
               (`iLt `iZero d
                 (r(`iAdd x `iOne)
                   (`iSub y `iOne)
                   (`iAdd d(`iAdd(`iMult `iFour (`iSub xy))
                                  `iEighteen))e)
                 (r(`iAdd x `iOne)
                    y
                    (`iAdd d(`iAdd(`iMult `iFour x)`iTen))e)))
             (`dSub (`dSqrt (`iToD (`iAdd (#square x) (#square y))))
                    (`iToD e))))]
 
         circle ::= [λpq.(λr.#concatMap (λx.(λabcd.
              #cons (#P (`iAdd ac) (`iAdd bd))
              (#cons (#P (`iSub ac) (`iAdd bd))
              (#cons (#P (`iSub ac) (`iSub bd))
              (#cons (#P (`iAdd ac) (`iSub bd))
              (#cons (#P (`iAdd ad) (`iAdd bc))
              (#cons (#P (`iSub ad) (`iAdd bc))
              (#cons (#P (`iSub ad) (`iSub bc))
              (#cons (#P (`iAdd ad) (`iSub bc))
              #nil)))))))
              )(#fst p)(#snd p)(#fst x)(#snd x))
           (#bresCircle `iZero r(`iSub `iThree(`iMult `iTwo r))))
           (`iSqrt (`iAdd
               (#square (`iSub (#fst p) (#fst q)))
               (#square (`iSub (#snd p) (#snd q))))) ]
         smoothCircle ::= [λpq.(λr.#concatMap (λx.(λabcde.
              #cons (#P (#P (`iAdd ac) (`iAdd bd))e)
              (#cons (#P (#P (`iSub ac) (`iAdd bd))e)
              (#cons (#P (#P (`iSub ac) (`iSub bd))e)
              (#cons (#P (#P (`iAdd ac) (`iSub bd))e)
              (#cons (#P (#P (`iAdd ad) (`iAdd bc))e)
              (#cons (#P (#P (`iSub ad) (`iAdd bc))e)
              (#cons (#P (#P (`iSub ad) (`iSub bc))e)
              (#cons (#P (#P (`iAdd ad) (`iSub bc))e)
              #nil)))))))
              )(#fst p)(#snd p)(#fst(#fst x))(#snd(#fst x))(#snd x))
           (#bresCircleSmooth `iZero r(`iSub `iThree(`iMult `iTwo r))r))
           (`iSqrt (`iAdd
               (#square (`iSub (#fst p) (#fst q)))
               (#square (`iSub (#snd p) (#snd q))))) ]
         `smooth
           (#toHListD(`fromShape `shape (λp.#map(λx.#P x `dOne)(#point p))
                                        (λpq.#map(λx.#P x `dOne)(#rect p q))
                                        (#line #T)
                                        (#poly #T)
                                        #smoothCircle))
           (#toHList(`fromShape `shape #point #rect
                                       (#line #F) (#poly #F) #circle))
      |]
    -- This just lets us use various Haskell things from λ-calculus
    shape = foreignDat hShape
    smooth = foreignData s
    fromShape = Foreign (toDyn f) [HaskellArg] False
      where
        f (Point hp) = let p = foreignData (pointToCoord z hp)
                       in [λ|λorlpc.o `p|]
        f (Rectangle hp hq) = let p = foreignData (pointToCoord z hp)
                                  q = foreignData (pointToCoord z hq)
                              in [λ|λorlpc.r `p `q|]
        f (Line hp hq) = let p = foreignData (pointToCoord z hp)
                             q = foreignData (pointToCoord z hq)
                         in [λ|λorlpc.l `p `q|]
        f (Polygon hps) = let ps = foreignData (map (pointToCoord z) hps)
                          in [λ|λorlpc.p `ps|]
        f (Circle hp hq) = let p = foreignData (pointToCoord z hp)
                               q = foreignData (pointToCoord z hq)
                           in [λ|λorlpc.c `p `q|]
    cons = Foreign (toDyn ((:) :: ((Int,Int),Double) ->
                                   [((Int,Int),Double)] ->
                                   [((Int,Int),Double)]))
           [HaskellArg, HaskellArg] True
    nil = Foreign (toDyn ([] :: [((Int,Int),Double)])) [] True
    pair1 = Foreign (toDyn ((,) :: Int -> Int -> (Int,Int)))
           [HaskellArg, HaskellArg] True
    pair2 = Foreign (toDyn ((,) :: (Int,Int) -> Double -> ((Int,Int),Double)))
           [HaskellArg, HaskellArg] True
    iSqrt = Foreign (toDyn (round.(sqrt::Double->Double).fromIntegral :: (Int -> Int)))
           [HaskellArg] True
    dSqrt = Foreign (toDyn ((sqrt::Double->Double) :: (Double -> Double)))
           [HaskellArg] True

    iToD = Foreign (toDyn (fromIntegral :: Int->Double)) [HaskellArg] True
    dFloor = Foreign (toDyn (floor :: Double->Int)) [HaskellArg] True
    iThree = foreignData (3 :: Int)
    iFour = foreignData (4 :: Int)
    iTen = foreignData (10 :: Int)
    iEighteen = foreignData (18 :: Int)
    -- This is easy to write in λ-calc, but the gimic here isn't
    -- worth the performance hit
    clean = go . sort
      where go [] = []
            go [x] = [x]
            go ((x,p):xs@((y,_):_))
              | x == y || p < 0 = go xs
              | otherwise = (x,min 1 p) : go xs
