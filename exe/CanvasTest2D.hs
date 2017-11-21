{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module CanvasTest2D (mainish) where

import           Control.Lens                   (Lens', both, cons, makeLenses,
                                                 makePrisms, over, to, uncons,
                                                 unsnoc, (%~), (&), (+~), (.~),
                                                 (^.), _1, _2)

import           Control.Monad.Fix              (MonadFix)

import           Reflex                         (Dynamic, Event, MonadHold,
                                                 Reflex, (<@), (<@>))
import           Reflex.Dom                     (MonadWidget)

import qualified Reflex                         as R
import qualified Reflex.Dom                     as RD

import           Data.Text                      (Text)
import qualified Data.Text                      as Text

import           Data.Foldable                  (traverse_)

import           Reflex.Dom.Canvas2DF           (CanvasM)
import qualified Reflex.Dom.Canvas2DF           as CanvasF

import qualified Reflex.Dom.CanvasBuilder.Types as Canvas
import qualified Reflex.Dom.CanvasDyn           as CDyn

import           Data.Time                      (UTCTime, getCurrentTime)

import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as S

import           System.Random                  (StdGen)
import qualified System.Random                  as Rnd

import qualified Data.Map                       as Map

#ifndef ghcjs_HOST_OS
import qualified Run
#endif

dFloatFeed
  :: ( Reflex t
     , MonadHold t m
     , MonadFix m
     )
  => (Float, Float)
  -> StdGen
  -> Event t ()
  -> m (Dynamic t Float)
dFloatFeed lohi stdGen eTickInfo =
  let
    rr = Rnd.randomR lohi
  in mdo
    dRGen <- R.holdDyn (rr stdGen)
      $ (rr . snd) <$> R.current dRGen <@ eTickInfo

    pure ( fst <$> dRGen )

keepFixedSeq
  :: Int
  -> a
  -> Seq a
  -> Seq a
keepFixedSeq limit n s
  | S.length s + 1 >= limit = maybe mempty (cons n . fst) $ unsnoc s
  | otherwise               = cons n s

data Point = Point Float Float
  deriving (Show)
makePrisms ''Point

data Line = Line
  { _line_start       :: Point
  , _line_end         :: Point
  , _line_strokeStyle :: Text
  }
  deriving (Show)
makeLenses ''Line

data DataSet = DataSet
  { _dataSet_max   :: Float
  , _dataSet_min   :: Float
  , _dataSet_lines :: Seq Line
  }
  deriving (Show)
makeLenses ''DataSet

lineInstruction
  :: Line
  -> CanvasM ()
lineInstruction line = do
  let
    sigh :: (Float,Float) -> (Double,Double)
    sigh = over both ( fromRational . toRational )

    f l g =
      uncurry g (line ^. l . _Point . to sigh)

  f line_start CanvasF.moveToF
  f line_end CanvasF.lineToF

zeroLine
  :: Line
zeroLine =
  let
    zeroPoint = Point 0.0 0.0
  in
    Line zeroPoint zeroPoint "#000000"

emptyDataSet
  :: DataSet
emptyDataSet = DataSet 0.0 0.0
  ( S.singleton zeroLine )

dDataz
  :: ( Reflex t
     , MonadHold t m
     , MonadFix m
     )
  => Int
  -> Int
  -> Int
  -> Event t Float
  -> m (Dynamic t DataSet)
dDataz _ w limit eNewDataPoint =
  let
    stepToTheRight = fromIntegral $ w `div` limit
  in
    R.foldDyn
    (\n ds -> ds
      & dataSet_max %~ max n
      & dataSet_min %~ min n
      & dataSet_lines . traverse %~
      ( ( line_end . _Point . _1 +~ stepToTheRight )
      . ( line_start . _Point . _1 +~ stepToTheRight )
      )
      & dataSet_lines %~ (\xs -> addNewDataPoint n xs $ uncons xs)
    )
    emptyDataSet
    eNewDataPoint
  where
    newLine p l = l
      & line_end .~ (l ^. line_start)
      & line_start . _Point . _2 .~ p
      & line_start . _Point . _1 .~ 0.0

    addNewDataPoint n s  Nothing       = keepFixedSeq limit (newLine n zeroLine) s
    addNewDataPoint n _ (Just (h', t)) = keepFixedSeq limit (newLine n h') (cons h' t)

eDraw
  :: MonadWidget t m
  => UTCTime
  -> StdGen
  -> m ()
eDraw aTime stdGen = do
  let
    canvasH = 480
    canvasW = 640
    dataN   = 20

    canvasId = "fruit-balls"
    canvasAttrs = pure $ Map.fromList
      [ ("height", "480")
      , ("width", "640")
      ]

  eStart <- RD.button "Start"
  eStop  <- RD.button "Stop"

  -- Create the canvas element
  canvasEl <- fst <$> RD.elDynAttr' "canvas"
    (Map.insert "id" canvasId <$> canvasAttrs) RD.blank

  -- Create our canvas painter, will be restricted to 'context2d' because of the types! :D
  d2D <- CDyn.dPaint2d $ Canvas.CanvasConfig canvasEl []

  eTick <- RD.tickLossy 0.016 aTime

  eTicken <- fmap R.switch . R.hold R.never $ R.leftmost
    [ ()      <$ eTick <$ eStart
    , R.never <$ eStop
    ]

  dFloatFeed' <- dFloatFeed ( 0.0, 450.0 ) stdGen eTicken

  dDataLines <- dDataz canvasH canvasW dataN
    $ R.current dFloatFeed' <@ eTicken

  let
    eLines = ( ^. dataSet_lines )
      <$> R.current dDataLines
      <@ eTicken

    toDoublePair :: Lens' Line Point -> Line -> (Double,Double)
    toDoublePair l = (^. l . _Point . to (over both ( fromRational . toRational )))

    moveDraw l = do
      uncurry CanvasF.moveToF $ line_start `toDoublePair` l
      uncurry CanvasF.lineToF $ line_end `toDoublePair` l

    toCM :: Seq Line -> CanvasM ()
    toCM xs = do
      CanvasF.clearRectF 0.0 0.0 (fromIntegral canvasW) (fromIntegral canvasH)
      CanvasF.beginPathF
      traverse_ moveDraw xs
      CanvasF.closePathF
      CanvasF.strokeStyleF "#000000"
      CanvasF.strokeF

    eLineIns = (\cx xs -> cx & Canvas.canvasPaint_actions .~ toCM xs)
      <$> R.current d2D
      <@> eLines

  _ <- CDyn.paintToCanvas eLineIns

  RD.el "div" $
    RD.dynText ( ( Text.pack . show ) <$> dDataLines )

#ifdef ghcjs_HOST_OS
mainish
  :: IO ()
mainish = do
  n <- getCurrentTime
  g <- Rnd.getStdGen
  RD.mainWidget ( eDraw n g )
#endif

#ifndef ghcjs_HOST_OS
mainish
  :: IO ()
mainish = do
  n <- getCurrentTime
  g <- Rnd.getStdGen
  Run.run ( eDraw n g )
#endif
