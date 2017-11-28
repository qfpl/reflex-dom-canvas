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
module Reflex.Dom.Canvas.Context2D where

import           Control.Lens                   (makeClassyPrisms, ( # ), (^?),
                                                 _2, _3)

import           Control.Monad.Free.Church      (F, foldF, liftF)

import           JSDOM.CanvasPath               as C
import           JSDOM.CanvasRenderingContext2D as C

import           JSDOM.Enums                    (CanvasWindingRule)
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
  | Fill CanvasWindingRule a
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
  | Clip CanvasWindingRule a
  -- | QuadraticCurveTo Double Double Double Double a
  -- | BezierCurveTo Doub But I might start building something a bit bigger than just little shapes or le Double Double Double Double Double a
  -- | Arc Double Double Double Double Double Bool a
  -- | ArcTo Double Double Double Double Double a
  | Rect Double Double Double Double a
  | ClearRect Float Float Float Float a
  | StrokeRect Float Float Float Float a
  -- | DrawImage CanvasImageSource Float Float a
  | Done a
  deriving (Functor, Foldable, Traversable, Show, Eq)
makeClassyPrisms ''CanvasF

type CanvasM = F CanvasF

-- instance AsCanvasF (CanvasM a) (CanvasM a) where
--   _CanvasF = _Free

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
  :: F CanvasF a
  -> F CanvasF a
batchDrawInstructions =
  error "batchDrawInstructions not implemented"

findDrawBounds
  :: CanvasF a
  -> (Float, Float, Float, Float)
findDrawBounds =
  error "drawBounds not implemented"

drawToCanvas
  :: MonadJSM m
  => F CanvasF a
  -> C.CanvasRenderingContext2D
  -> m a
drawToCanvas instructions cxt =
  foldF ( applyInstruction cxt ) instructions

applyInstruction :: MonadJSM m => C.CanvasRenderingContext2D -> CanvasF a -> m a
applyInstruction cxt instruction =
  case instruction of
    BeginPath cont             -> cont <$ C.beginPath cxt
    ClearRect x y w h cont     -> cont <$ C.clearRect cxt x y w h
    Clip rule cont             -> cont <$ C.clip cxt (Just rule)
    ClosePath cont             -> cont <$ C.closePath cxt
    Fill rule cont             -> cont <$ C.fill cxt ( Just rule )
    FillRect x y w h cont      -> cont <$ C.fillRect cxt x y w h
    LineTo x y cont            -> cont <$ C.lineTo cxt x y
    MoveTo x y cont            -> cont <$ C.moveTo cxt x y
    Rect x y w h cont          -> cont <$ C.rect cxt x y w h
    Stroke cont                -> cont <$ C.stroke cxt
    StrokeRect x y w h cont    -> cont <$ C.strokeRect cxt x y w h
    StrokeStyle style cont     -> cont <$ C.setStrokeStyle cxt style
    Transform a b c d e f cont -> cont <$ C.transform cxt a b c d e f

    Done a                     -> pure a

    --  FillText text x y cont                                -> cont <$ C.fillText text x y
    --  Font font' cont                                       -> cont <$ C.font font'
    --  GlobalAlpha value cont                                -> cont <$ C.globalAlpha value
    --  LineCap linecap cont                                  -> cont <$ C.lineCap linecap
    --  LineDashOffset offset cont                            -> cont <$ C.lineDashOffset offset
    --  LineJoin linejoin cont                                -> cont <$ C.lineJoin linejoin
    --  LineWidth width cont                                  -> cont <$ C.lineWidth width
    --  MeasureText text cont                                 -> cont <$ C.measureText text
    --  MiterLimit limit cont                                 -> cont <$ C.miterLimit limit
    --  Rotate angle cont                                     -> cont <$ C.rotate angle
    --  Scale x y cont                                        -> cont <$ C.scale x y
    --  SetLineDash distances cont                            -> cont <$ C.setLineDash distances
    --  SetTransform a b c d e f cont                         -> cont <$ C.setTransform a b c d e f
    --  StrokeText text x y cont                              -> cont <$ C.strokeText text x y
    --  TextAlign alignment cont                              -> cont <$ C.textAlign alignment
    --  TextBaseline baseline cont                            -> cont <$ C.textBaseline baseline
    --  Translate x y cont                                    -> cont <$ C.translate x y
    --  DrawImage img dw dh cont                              -> cont <$ C.drawImage cxt img dw dh
    --  FillRule rule cont                                    -> cont <$ C.setFillRule cxt rule
    --  FillStyle style cont                                  -> cont <$ C.setFillStyle cxt style
    --  Arc x y radius startAngle endAngle anticlockwise cont
    --    -> cont <$ C.arc x y radius startAngle endAngle anticlockwise
    --  ArcTo cp1_X cp1_Y cp2_X cp2_Y radius cont
    --    -> cont <$ C.arcTo cp1_X cp1_Y cp2_X cp2_Y radius
    --  BezierCurveTo cp1_X cp1_Y cp2_X cp2_Y endX endY cont
    --    -> cont <$ C.bezierCurveTo cp1_X cp1_Y cp2_X cp2_Y endX endY
    --  QuadraticCurveTo cpX cpY endX endY cont
    --    -> cont <$ C.quadraticCurveTo cpX cpY endX endY


fillF :: CanvasWindingRule -> CanvasM ()
fillF rule = liftF $ Fill rule ()

strokeF :: CanvasM ()
strokeF = liftF $ Stroke ()

strokeStyleF :: JSString -> CanvasM ()
strokeStyleF style = liftF $ StrokeStyle style ()

beginPathF :: CanvasM ()
beginPathF = liftF $ BeginPath ()

closePathF :: CanvasM ()
closePathF = liftF $ ClosePath ()

clipF :: CanvasWindingRule -> CanvasM ()
clipF rule = liftF $ Clip rule ()

doneF :: CanvasM ()
doneF = liftF $ Done ()

moveToF :: Double -> Double -> CanvasM ()
moveToF x y = liftF $ MoveTo x y ()

lineToF :: Double -> Double -> CanvasM ()
lineToF x y = liftF $ LineTo x y ()

clearRectF, fillRectF, strokeRectF :: Float -> Float -> Float -> Float -> CanvasM ()
clearRectF x y w h  = liftF $ ClearRect x y w h ()
fillRectF x y w h   = liftF $ FillRect x y w h ()
strokeRectF x y w h = liftF $ StrokeRect x y w h ()


drawOneLine
  :: F CanvasF ()
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
  :: F CanvasF ()
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
