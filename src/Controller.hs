--- Copyright 2018 The Australian National University, All rights reserved
module Controller where

import CodeWorld hiding (Point)
import Model

import Data.Char (isDigit)
import Data.Text (pack, unpack)

handleTime :: Double -> Model -> Model
handleTime = flip const

handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss z s t) =
  case event of
    KeyPress key
      | k == "Esc" -> initialModel
        -- revert to an empty canvas
      | k == "D" -> trace (pack $ show m) m
        --   the current model on the console
      | k == "S" -> Model ss z (not s) t
        -- turn smoothing on/off
      | k == "0" -> Model ss (1 / 10) s t
        -- set the pixel resolution to 0.1
      | isDigit . head $ k -> Model ss (1 / read [head k]) s t
        -- set the pixel resolution to 1/k
      | k == "-" || k == "," -> Model ss (z / 2.0) s t
        -- halve the pixel resolution
      | k == "=" || k == "." -> Model ss (z * 2.0) s t
        -- double the pixel resolution
      | k == "Backspace" || k == "Delete" -> Model (drop 1 ss) z s t
        -- drop the last added shape
      | k == " " ->
        case t of
          Just (PolygonTool l@(_:_:_:_)) -- polygons have at least three vertices
           -> Model (Polygon l : ss) z s Nothing
              -- add a polygon
          _ -> m -- not enough vertices yet
      | otherwise -> maybe m (Model ss z s . Just) (selectTool k)
      where k = unpack key
    MousePress btn p
      | btn == LeftButton -> maybe m (Model ss z s . Just) (updateTool t p)
      | otherwise -> m
    MouseRelease btn p
      | btn == LeftButton ->
        maybe m (\shape -> Model (shape : ss) z s Nothing) (triggerTool t p)
      | otherwise -> m
    _ -> m

selectTool :: String -> Maybe Tool
selectTool k =
  case k of
    "O" -> Just $ PointTool Nothing
    "R" -> Just $ RectangleTool Nothing
    "L" -> Just $ LineTool Nothing
    "C" -> Just $ CircleTool Nothing
    "P" -> Just $ PolygonTool []
    _ -> Nothing

updateTool :: Maybe Tool -> Point -> Maybe Tool
updateTool t p =
  case t of
    Just (PointTool _) -> Just $ PointTool $ Just p
    Just (PolygonTool vs) -> Just $ PolygonTool $ p : vs
    Just (RectangleTool _) -> Just $ RectangleTool $ Just p
    Just (LineTool _) -> Just $ LineTool $ Just p
    Just (CircleTool _) -> Just $ CircleTool $ Just p
    _ -> t

triggerTool :: Maybe Tool -> Point -> Maybe Shape
triggerTool t p2 =
  case t of
    Just (PolygonTool _) -> Nothing
    Just (PointTool (Just p1)) -> Just $ Point p1
    Just (RectangleTool (Just p1)) -> Just $ Rectangle p1 p2
    Just (LineTool (Just p1)) -> Just $ Line p1 p2
    Just (CircleTool (Just p1)) -> Just $ Circle p1 p2
    _ -> Nothing
