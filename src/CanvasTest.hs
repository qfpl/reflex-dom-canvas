{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module CanvasTest (mainish) where

import           Control.Monad.Primitive        (PrimMonad)

import           Control.Lens                   (both, cons, makeLenses,
                                                 makePrisms, over, to,
                                                 traverseOf_, uncons, unsnoc,
                                                 (%~), (&), (+~), (.~), (^.),
                                                 _1, _2)

import           Control.Monad.Fix              (MonadFix)

import           Reflex                         (Dynamic, Event, MonadHold,
                                                 Reflex, (<@))
import           Reflex.Dom                     (MonadWidget)

import qualified Reflex                         as R
import qualified Reflex.Dom                     as RD

import           Data.Text                      (Text)
import qualified Data.Text                      as Text

import           Data.Time                      (UTCTime, getCurrentTime)

import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as S

import           System.Random                  (StdGen)
import qualified System.Random                  as Rnd

import qualified Data.Map                       as Map

import qualified JSDOM.Types                    as JSDOM
import qualified JSDOM.Enums                    as JSEnums

import qualified Reflex.Dom.Canvas2DF           as CanvasF
import qualified Reflex.Dom.CanvasBuilder       as CB
import           Reflex.Dom.CanvasBuilder.Types (canvasPaint_actions)

import qualified Reflex.Dom.CanvasDyn as CD

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

  -- eStart <- RD.button "Start"
  -- eStop  <- RD.button "Stop"

  -- Create the canvas element
  canvasEl <- fst <$> RD.elDynAttr' "canvas"
    (Map.insert "id" canvasId <$> canvasAttrs) RD.blank

  -- eTick <- (() <$) <$> RD.tickLossy 0.100 aTime

  -- eTicken <- fmap R.switch . R.hold eTick $ R.leftmost
  --   [ eTick   <$ eStart
  --   , R.never <$ eStop
  --   ]

  -- dFloatFeed' <- dFloatFeed ( 0.0, 450.0 ) stdGen eTicken

  -- dDataLines <- dDataz canvasH canvasW dataN
  --   $ R.current dFloatFeed' <@ eTicken

  dCanvas2d <- CD.dPaint2d ( CB.CanvasConfig canvasEl mempty )

  let
    x = canvasW / 2
    y = canvasH / 2

    cSquare = do
      CanvasF.clearRectF 0.0 0.0 canvasW canvasH
      CanvasF.fillRectF x y 100.0 100.0
      CanvasF.clearRectF  (x + 20.0) (y + 20.0) 60.0 60.0
      CanvasF.strokeRectF (x + 25.0) (y + 25.0) 50.0 50.0

    cTriangle = do
      CanvasF.clearRectF 0.0 0.0 canvasW canvasH
      CanvasF.beginPathF
      CanvasF.moveToF 50.0 75.0
      CanvasF.lineToF 100.0 25.0
      CanvasF.lineToF 100.0 75.0
      CanvasF.closePathF
      CanvasF.fillF JSEnums.CanvasWindingRuleNonzero

  eSquare <- RD.button "Square"
  eTriangle <- RD.button "Triangle"

  dShape <- R.holdDyn cTriangle $ R.leftmost
    [ cSquare <$ eSquare
    , cTriangle <$ eTriangle
    ]

  let
    dCanvasActions =
      (canvasPaint_actions .~) <$> dShape <*> dCanvas2d

    -- dCanvasActions = R.zipDynWith
    --   (\ds -> canvasPaint_actions .~ drawPlotLines ds)
    --   dDataLines
    --   dCanvas2d

  _ <- RD.dyn ( CD.paintToCanvas <$> dCanvasActions )

  pure ()

  -- let
  --   cb = CB.with2DContext ( CB.CanvasConfig canvasEl mempty )

  -- eCanvasDraw <- RD.dyn ( cb . drawToCanvasM <$> dDataLines )

  -- RD.el "div" $ do
  --   RD.text "Max Value: "
  --   RD.dynText ( ( Text.pack . show . _dataSet_max ) <$> dDataLines )
  -- RD.el "div" $ do
  --   RD.text "Min Value: "
  --   RD.dynText ( ( Text.pack . show . _dataSet_min ) <$> dDataLines )

drawToCanvasM
  :: CB.Monad2DCanvas t m
  => DataSet
  -> m ()
drawToCanvasM =
  CB.liftCx2d . drawPlotLines

drawPlotLines
  :: DataSet
  -> CanvasF.CanvasM ()
drawPlotLines ds = do
  let
    sigh :: (Float,Float) -> (Double,Double)
    sigh = over both ( fromRational . toRational )

    moveDraw l = do
      uncurry CanvasF.moveToF $ l ^. line_start . _Point . to sigh
      uncurry CanvasF.lineToF $ l ^. line_end . _Point . to sigh

  CanvasF.clearRectF 0.0 0.0 640.0 480.0
  CanvasF.beginPathF

  traverseOf_ ( dataSet_lines . traverse ) moveDraw ds

  CanvasF.closePathF
  CanvasF.strokeStyleF "#000000"
  CanvasF.strokeF

-- moveToM,lineToM :: CB.Monad2DCanvas t m => Double -> Double -> m ()
-- moveToM x y = CB.liftCx (\c -> CR.moveTo c x y )
-- lineToM x y = CB.liftCx (\c -> CR.lineTo c x y )

-- strokeStyleM :: CB.Monad2DCanvas t m => JSDOM.JSString -> m ()
-- strokeStyleM s = CB.liftCx (`CR.setStrokeStyle` s)

-- beginPathM,closePathM,strokeM :: CB.Monad2DCanvas t m => m ()
-- beginPathM = CB.liftCx CR.beginPath
-- closePathM = CB.liftCx CR.closePath
-- strokeM    = CB.liftCx CR.stroke

-- clearRectM :: CB.Monad2DCanvas t m => Float -> Float -> Float -> Float -> m ()
-- clearRectM x y w h = CB.liftCx (\c -> CR.clearRect c x y w h)

-- drawToCanvasDirect
--   :: CB.Monad2DCanvas t m
--   => DataSet
--   -> m ()
-- drawToCanvasDirect ds = do
--   let
--     sigh :: (Float,Float) -> (Double,Double)
--     sigh = over both ( fromRational . toRational )

--     moveDraw l = do
--       uncurry moveToM $ l ^. line_start . _Point . to sigh
--       uncurry lineToM $ l ^. line_end . _Point . to sigh

--   clearRectM 0.0 0.0 640.0 480.0
--   beginPathM

--   traverseOf_ ( dataSet_lines . traverse ) moveDraw ds

--   closePathM
--   strokeStyleM "#000000"
--   strokeM

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
