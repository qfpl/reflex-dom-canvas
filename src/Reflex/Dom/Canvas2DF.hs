{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
module Reflex.Dom.Canvas2DF where

import           Control.Lens                   (makeClassyPrisms, ( # ), (^?),
                                                 _2, _3)

import           Control.Monad.Free             (Free, foldFree, liftF, _Free)

import           JSDOM.CanvasPath               as C
import           JSDOM.CanvasRenderingContext2D as C

import           JSDOM.Types                    (JSString, MonadJSM)

-- Disallow because we want to control this externally
-- Create Int Int (Canvas -> a)
-- GetContext Canvas (Context -> a)
-- Width Canvas (Int -> a)
-- Height Canvas (Int -> a)
-- SetHeight Int Canvas a
-- SetWidth Int Canvas a

-- Disallow due to expense, for now. Has great use for preserving transforms
-- for child layers, given that the canvas is a giant global monster however.
-- That may just never work.
-- Save Context (Context -> a)
 -- Restore Context (Context -> a)

-- Isn't there...
-- CreatePattern Image Repeat (Pattern -> a)
-- Has the wrong type...
-- IsPointInPath Double Double (Bool -> a)

data CanvasF a
  = Transform Float Float Float Float Float Float a
  -- | SetTransform Double Double Double Double Double Double a
  -- | Scale Double Double a
  -- | Translate Double Double a
  -- | Rotate Double a
  | Fill a
  -- | FillRule JSString a
  -- | FillStyle CanvasStyle a
  -- | GlobalAlpha Double a
  -- | LineJoin C.LineJoin a
  -- | LineCap C.LineCap a
  -- | MiterLimit Double a
  -- | SetLineDash JSArray a
  -- | LineDashOffset Double a
  -- | TextAlign C.TextAlign a
  -- | TextBaseline C.TextBaseline a
  -- | LineWidth Double a
  -- | Font JSString a
  -- | MeasureText JSString (Double -> a)
  | FillRect Float Float Float Float a
  -- | FillText JSString Double Double a
  -- | StrokeText JSString Double Double a
  | BeginPath a
  | MoveTo Double Double a
  | LineTo Double Double a
  | ClosePath a
  | StrokeStyle JSString a
  | Stroke a
  | Clip a
  -- | QuadraticCurveTo Double Double Double Double a
  -- | BezierCurveTo Double Double Double Double Double Double a
  -- | Arc Double Double Double Double Double Bool a
  -- | ArcTo Double Double Double Double Double a
  | Rect Double Double Double Double a
  | ClearRect Float Float Float Float a
  -- | StrokeRect Double Double Double Double a
  -- | DrawImage CanvasImageSource Float Float a
  | Done a
  deriving (Functor, Foldable, Traversable, Show, Eq)
makeClassyPrisms ''CanvasF

type CanvasM = Free CanvasF

instance AsCanvasF (CanvasM a) (CanvasM a) where
  _CanvasF = _Free

-- Canvas doesn't always play well with floating point coordinates, according to:
-- https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial/Optimizing_canvas
-- It is better to provide these values as integers, or whole numbers.
--
-- I'm not convinced this is a good idea. But it's a nice simple 'Plated' example for me.
-- truncateDrawImageCoords
--   :: CanvasF a
--   -> CanvasF a
-- truncateDrawImageCoords =
--   let
--     moosh = fromInteger . truncate
--   in
--     P.transform $ \case
--       DrawImage img x y n -> DrawImage img (moosh x) (moosh y) n
--       c                   -> c

-- Simply put... Provided Stroke properties match, then:
-- [ BeginPath [MoveTo,LineTo] ClosePath ( StrokeStyle StyleX ) Stroke ]
-- =
-- [ BeginPath [MoveTo,LineTo] ClosePath ] ( StrokeStyle StyleX ) Stroke
--
batchDrawInstructions
  :: Free CanvasF a
  -> Free CanvasF a
batchDrawInstructions =
  error "batchDrawInstructions not implemented"

findDrawBounds
  :: CanvasF a
  -> (Float, Float, Float, Float)
findDrawBounds =
  error "drawBounds not implemented"

drawToCanvas
  :: MonadJSM m
  => Free CanvasF a
  -> C.CanvasRenderingContext2D
  -> m a
drawToCanvas instructions cxt =
  foldFree ( applyInstruction cxt ) instructions

applyInstruction :: MonadJSM m => C.CanvasRenderingContext2D -> CanvasF a -> m a
applyInstruction cxt instruction =
  case instruction of
    Transform a b c d e f cont -> cont <$ C.transform cxt a b c d e f
    --  SetTransform a b c d e f cont                          -> cont <$ C.setTransform a b c d e f
    --  Scale x y cont                                         -> cont <$ C.scale x y
    --  Translate x y cont                                     -> cont <$ C.translate x y
    --  Rotate angle cont                                      -> cont <$ C.rotate angle
    Fill cont                  -> cont <$ C.fill cxt Nothing
    --  FillRule rule cont                                     -> cont <$ C.fillRule rule
    -- FillStyle style cont       -> cont <$ C.setFillStyle cxt style
    StrokeStyle style cont     -> cont <$ C.setStrokeStyle cxt style
    --  GlobalAlpha value cont                                 -> cont <$ C.globalAlpha value
    --  LineJoin linejoin cont                                 -> cont <$ C.lineJoin linejoin
    --  LineCap linecap cont                                   -> cont <$ C.lineCap linecap
    --  MiterLimit limit cont                                  -> cont <$ C.miterLimit limit
    --  SetLineDash distances cont                             -> cont <$ C.setLineDash distances
    --  LineDashOffset offset cont                             -> cont <$ C.lineDashOffset offset
    --  TextAlign alignment cont                               -> cont <$ C.textAlign alignment
    --  TextBaseline baseline cont                             -> cont <$ C.textBaseline baseline
    --  LineWidth width cont                                   -> cont <$ C.lineWidth width
    --  Font font' cont                                        -> cont <$ C.font font'
    --  MeasureText text cont                                  -> cont <$ C.measureText text
    FillRect x y w h cont      -> cont <$ C.fillRect cxt x y w h
    --  FillText text x y cont                                 -> cont <$ C.fillText text x y
    --  StrokeText text x y cont                               -> cont <$ C.strokeText text x y
    Stroke cont                -> cont <$ C.stroke cxt
    BeginPath cont             -> cont <$ C.beginPath cxt
    ClosePath cont             -> cont <$ C.closePath cxt
    Clip cont                  -> cont <$ C.clip cxt Nothing
    MoveTo x y cont            -> cont <$ C.moveTo cxt x y
    LineTo x y cont            -> cont <$ C.lineTo cxt x y
    --  QuadraticCurveTo cpX cpY endX endY cont                -> cont <$ C.quadraticCurveTo cpX cpY endX endY
    --  BezierCurveTo cp1_X cp1_Y cp2_X cp2_Y endX endY cont   -> cont <$ C.bezierCurveTo cp1_X cp1_Y cp2_X cp2_Y endX endY
    --  Arc x y radius startAngle endAngle anticlockwise cont  -> cont <$ C.arc x y radius startAngle endAngle anticlockwise
    --  ArcTo cp1_X cp1_Y cp2_X cp2_Y radius cont              -> cont <$ C.arcTo cp1_X cp1_Y cp2_X cp2_Y radius
    Rect x y w h cont          -> cont <$ C.rect cxt x y w h
    ClearRect x y w h cont     -> cont <$ C.clearRect cxt x y w h
    --  StrokeRect x y w h cont                                -> cont <$ C.strokeRect x y w h
    -- DrawImage img dw dh cont   -> cont <$ C.drawImage cxt img dw dh
    Done a                     -> pure a


strokeF :: CanvasM ()
strokeF = liftF $ Stroke ()

strokeStyleF :: JSString -> CanvasM ()
strokeStyleF style = liftF $ StrokeStyle style ()

beginPathF :: CanvasM ()
beginPathF = liftF $ BeginPath ()

closePathF :: CanvasM ()
closePathF = liftF $ ClosePath ()

clipF :: CanvasM ()
clipF = liftF $ Clip ()

doneF :: CanvasM ()
doneF = liftF $ Done ()

moveToF :: Double -> Double -> CanvasM ()
moveToF x y = liftF $ MoveTo x y ()

lineToF :: Double -> Double -> CanvasM ()
lineToF x y = liftF $ LineTo x y ()

clearRectF :: Float -> Float -> Float -> Float -> CanvasM ()
clearRectF x y w h = liftF $ ClearRect x y w h ()

drawOneLine
  :: Free CanvasF ()
drawOneLine = do
  beginPathF
  moveToF 10 10
  lineToF 20 20
  closePathF
  strokeStyleF "#0ff"
  strokeF
  doneF

batchLineDraw
  :: AsCanvasF s s => s -> Maybe s
batchLineDraw s = do
  let
    oneLineP =
      _BeginPath . _MoveTo . _3 . _LineTo . _3 . _ClosePath . _StrokeStyle . _2 . _Stroke

  tail' <- s ^? oneLineP . oneLineP

  (move1X, move1Y, _) <- s ^? _BeginPath . _MoveTo
  (line1X, line1Y, _) <- s ^? _BeginPath . _MoveTo . _3 . _LineTo

  (sStyle1, _) <- s ^? _BeginPath . _MoveTo . _3 . _LineTo . _3 . _ClosePath . _StrokeStyle

  (move2X, move2Y, _) <- s ^? oneLineP . _BeginPath . _MoveTo
  (line2X, line2Y, _) <- s ^? oneLineP . _BeginPath . _MoveTo . _3 . _LineTo

  (sStyle2, _) <- s ^? oneLineP . _BeginPath . _MoveTo . _3 . _LineTo . _3 . _ClosePath . _StrokeStyle

  if sStyle1 /= sStyle2 then Nothing
  else Just $
    _BeginPath # (
      _MoveTo # (move1X, move1Y,
        _LineTo # (line1X, line1Y,
          _MoveTo # (move2X, move2Y,
            _LineTo # (line2X, line2Y,
              _ClosePath # (
                _StrokeStyle # (sStyle1, _Stroke # tail')))))))

drawMoreLinePoorly
  :: Free CanvasF ()
drawMoreLinePoorly = do
  let sty = "#0FF"
  beginPathF
  moveToF 10 10
  lineToF 10 10
  closePathF
  strokeStyleF sty
  strokeF
  beginPathF
  moveToF 20 10
  lineToF 20 10
  closePathF
  strokeStyleF sty
  strokeF
  beginPathF
  moveToF 20 20
  lineToF 20 20
  closePathF
  strokeStyleF sty
  strokeF
  doneF
