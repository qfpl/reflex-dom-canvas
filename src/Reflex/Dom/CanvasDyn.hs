{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Reflex.Dom.CanvasDyn
  ( dPaint2d
  , dPaintWebgl
  , paintToCanvas
  , applyToCanvasOn
  , dGLCx
  , drawGL
  ) where

import           Control.Lens                   ((^.))
import           Data.Coerce                    (coerce)
import           Data.Proxy                     (Proxy (..))
import           GHC.TypeLits                   (KnownSymbol, symbolVal)

import qualified JSDOM
import           JSDOM.HTMLCanvasElement        (getContextUnchecked)
import           JSDOM.Types                    (IsRenderingContext, JSM,
                                                 RenderingContext (..),
                                                 fromJSValUnchecked, liftJSM,
                                                 toJSVal)

import JSDOM.WebGLRenderingContextBase (WebGLRenderingContextBase)

import           Reflex                         (Event, Dynamic)

import           Reflex.Dom                     (MonadWidget)
import qualified Reflex.Dom                     as RD

import           Reflex.Dom.CanvasBuilder.Types

dCanvasPaint
  :: forall c t m a. ( MonadWidget t m
                     , KnownSymbol (RenderContextEnum c)
                     , IsRenderingContext (RenderContext c)
                     , HasRenderFn c
                     )
  => CanvasConfig c t
  -> m (Dynamic t ( CanvasPaint c t m a ))
dCanvasPaint cfg = do
  let
    reflexEl = cfg ^. canvasConfig_El
    cxType   = symbolVal ( Proxy :: Proxy (RenderContextEnum c) )

  renderFn <- liftJSM $ do
    e  <- fromJSValUnchecked =<< toJSVal ( RD._element_raw reflexEl )
    cx <- getContextUnchecked e cxType (cfg ^. canvasConfig_Args)
    pure $ renderFunction (Proxy :: Proxy c) ( coerce cx )

  let
    nextAnim a = liftJSM $
      JSDOM.nextAnimationFrame (\_ -> renderFn a )

  return . pure $ CanvasPaint
    nextAnim
    (`RD.keypress` reflexEl)

applyToCanvasOn
  :: MonadWidget t m
  => RenderFree c a
  -> Event t ( CanvasPaint (c :: ContextType) t m a )
  -> m (Event t a)
applyToCanvasOn instructions ePaint =
  let
    applyPaint cp =
      _canvasPaint_paint cp instructions
  in
    RD.performEvent ( applyPaint <$> ePaint )

paintToCanvas
  :: MonadWidget t m
  => Event t ( CanvasPaint (c :: ContextType) t m (), RenderFree c () )
  -> m ()
paintToCanvas ePaint =
  let
    applyPaint (cp, ins) =
      _canvasPaint_paint cp ins
  in
    RD.performEvent_ ( applyPaint <$> ePaint )

dPaint2d
  :: MonadWidget t m
  => CanvasConfig 'TwoD t
  -> m (Dynamic t (CanvasPaint 'TwoD t m a))
dPaint2d = dCanvasPaint

dPaintWebgl
  :: MonadWidget t m
  => CanvasConfig 'Webgl t
  -> m (Dynamic t (CanvasPaint 'Webgl t m a))
dPaintWebgl = dCanvasPaint

dCanvasCx
  :: forall c t m a. ( MonadWidget t m
                     , KnownSymbol (RenderContextEnum c)
                     , IsRenderingContext (RenderContext c)
                     , HasRenderFn c
                     )
  => CanvasConfig c t
  -> m (Dynamic t ( RenderContext c ))
dCanvasCx cfg = do
  let
    reflexEl = cfg ^. canvasConfig_El
    cxType   = symbolVal ( Proxy :: Proxy (RenderContextEnum c) )

  renderCx <- liftJSM $ do
    e  <- fromJSValUnchecked =<< toJSVal ( RD._element_raw reflexEl )
    getContextUnchecked e cxType (cfg ^. canvasConfig_Args)

  return $ pure ( coerce renderCx )

dGLCx
  :: MonadWidget t m
  => CanvasConfig 'Webgl t
  -> m (Dynamic t ( RenderContext 'Webgl ))
dGLCx =
  dCanvasCx

drawGL
  :: MonadWidget t m
  => RenderFree 'Webgl a
  -> Event t ( RenderContext 'Webgl )
  -> m (Event t a)
drawGL ins eCx =
  let
    rndr = renderFunction (Proxy::Proxy 'Webgl)

    nextFrame cx = liftJSM $
      JSDOM.nextAnimationFrame (\_ ->  rndr cx ins )
  in
    RD.performEvent ( nextFrame <$> eCx )
