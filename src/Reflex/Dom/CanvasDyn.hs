{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.CanvasDyn
  ( dPaint2d
  , dPaintWebgl
  , paintToCanvas
  ) where

import           Control.Lens                   ((^.))
import           Data.Coerce                    (coerce)
import           Data.Proxy                     (Proxy (..))
import           GHC.TypeLits                   (KnownSymbol, symbolVal)

import qualified JSDOM
import           JSDOM.HTMLCanvasElement        (getContextUnchecked)
import           JSDOM.Types                    (IsRenderingContext,
                                                 RenderingContext (..),
                                                 fromJSValUnchecked, liftJSM,
                                                 toJSVal)

import           Reflex                         (Dynamic)

import           Reflex.Dom                     (MonadWidget)
import qualified Reflex.Dom                     as RD

import qualified Reflex.Dom.Canvas2DF           as CanvasF

import           Reflex.Dom.CanvasBuilder.Types

dCanvasPaint
  :: forall c t m. ( MonadWidget t m
                   , KnownSymbol (RenderContextEnum c)
                   , IsRenderingContext (RenderContext c)
                   , HasRenderFn c
                   )
  => CanvasConfig c t
  -> m (Dynamic t ( CanvasPaint c t m ))
dCanvasPaint cfg = do
  let
    reflexEl = cfg ^. canvasConfig_El
    cxType   = symbolVal ( Proxy :: Proxy (RenderContextEnum c) )

  htmlCanvas <- liftJSM
    $ fromJSValUnchecked =<< toJSVal ( RD._element_raw reflexEl )

  canvasCx <- liftJSM
    $ getContextUnchecked htmlCanvas cxType (cfg ^. canvasConfig_Args)

  let
    nextAnim a = liftJSM $ JSDOM.nextAnimationFrame
      (\_ -> renderFunction (Proxy :: Proxy c) ( coerce canvasCx ) a)

  return . pure $ CanvasPaint nextAnim CanvasF.doneF
    (`RD.keypress` reflexEl)

paintToCanvas
  :: MonadWidget t m
  => CanvasPaint (c :: ContextType) t m
  -> m ()
paintToCanvas CanvasPaint {..} =
  _canvasPaint_paint _canvasPaint_actions

dPaint2d
  :: MonadWidget t m
  => CanvasConfig 'TwoD t
  -> m (Dynamic t (CanvasPaint 'TwoD t m))
dPaint2d = dCanvasPaint

dPaintWebgl
  :: MonadWidget t m
  => CanvasConfig 'Webgl t
  -> m (Dynamic t (CanvasPaint 'Webgl t m))
dPaintWebgl = dCanvasPaint
