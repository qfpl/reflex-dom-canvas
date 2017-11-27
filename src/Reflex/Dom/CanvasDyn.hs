{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Reflex.Dom.CanvasDyn
  ( dPaintContext2d
  , dPaintWebgl
  , drawGL
  , drawContext2d
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

import           Reflex                         (Event, Dynamic, (<@))
import qualified Reflex as R

import           Reflex.Dom                     (MonadWidget)
import qualified Reflex.Dom                     as RD

import           Reflex.Dom.CanvasBuilder.Types

dCanvasCx
  :: forall c t m. ( MonadWidget t m
                   , KnownSymbol (RenderContextEnum c)
                   , IsRenderingContext (RenderContext c)
                   , HasRenderFn c
                   )
  => CanvasConfig c t
  -> m ( Dynamic t ( CanvasInfo c t ) )
dCanvasCx cfg = do
  let
    reflexEl = cfg ^. canvasConfig_El
    cxType   = symbolVal ( Proxy :: Proxy (RenderContextEnum c) )

  renderCx <- liftJSM $ do
    e  <- fromJSValUnchecked =<< toJSVal ( RD._element_raw reflexEl )
    getContextUnchecked e cxType (cfg ^. canvasConfig_Args)

  return . pure $ CanvasInfo reflexEl ( coerce renderCx ) (`RD.keypress` reflexEl)

dPaintContext2d
  :: MonadWidget t m
  => CanvasConfig 'TwoD t
  -> m ( Dynamic t ( CanvasInfo 'TwoD t ) )
dPaintContext2d = dCanvasCx

dPaintWebgl
  :: MonadWidget t m
  => CanvasConfig 'Webgl t
  -> m ( Dynamic t ( CanvasInfo 'Webgl t ) )
dPaintWebgl = dCanvasCx

-- What I want to write... Not enough info about 'c', so I can't.
-- drawCanvasFree
--   :: ( MonadWidget t m
--      , HasRenderFn c
--      , IsRenderingContext ( RenderContext c ) ~ IsRenderingContext cx
--      )
--   => Dynamic t ( RenderFree c a )
--   -> Dynamic t ( RenderContext c )
--   -> Event t ()
--   -> m (Event t a)
-- drawCanvasFree dInstructions dContext eDraw =
--   let
--     rndr = renderFunction (Proxy :: Proxy c)

--     nextFrame cx ins = liftJSM $
--       JSDOM.nextAnimationFrame (\_ ->  rndr cx ins )
--   in
--     RD.performEvent
--     ( nextFrame
--       <$> R.current dContext
--       <*> R.current dInstructions
--       <@  eDraw
--     )

drawGL
  :: MonadWidget t m
  => Dynamic t ( RenderFree 'Webgl a )
  -> Dynamic t ( RenderContext 'Webgl )
  -> Event t ()
  -> m (Event t a)
drawGL dInstructions dContext eDraw =
  let
    rndr = renderFunction (Proxy :: Proxy 'Webgl)

    nextFrame cx ins = liftJSM $
      JSDOM.nextAnimationFrame (\_ ->  rndr cx ins )
  in
    RD.performEvent
    ( nextFrame
      <$> R.current dContext
      <*> R.current dInstructions
      <@  eDraw
    )

drawContext2d
  :: MonadWidget t m
  => Dynamic t ( RenderFree 'TwoD a )
  -> Dynamic t ( RenderContext 'TwoD )
  -> Event t ()
  -> m (Event t a)
drawContext2d dInstructions dContext eDraw =
  let
    rndr = renderFunction (Proxy :: Proxy 'TwoD)

    nextFrame cx ins = liftJSM $
      JSDOM.nextAnimationFrame (\_ ->  rndr cx ins )
  in
    RD.performEvent
    ( nextFrame
      <$> R.current dContext
      <*> R.current dInstructions
      <@  eDraw
    )

  -- let
  --   rndr = renderFunction (Proxy::Proxy 'Webgl)

  --   nextFrame cx ins = liftJSM $
  --     JSDOM.nextAnimationFrame (\_ ->  rndr cx ins )
  -- in
  --   RD.performEvent
  --   ( nextFrame
  --     <$> R.current dContext
  --     <*> R.current dInstructions
  --     <@  eDraw
  --   )

-- applyToCanvasOn
--   :: MonadWidget t m
--   => RenderFree c a
--   -> Event t ( CanvasPaint (c :: ContextType) t m a )
--   -> m (Event t a)
-- applyToCanvasOn instructions ePaint =
--   let
--     applyPaint cp =
--       _canvasPaint_paint cp instructions
--   in
--     RD.performEvent ( applyPaint <$> ePaint )

-- paintToCanvas
--   :: MonadWidget t m
--   => Event t ( CanvasPaint (c :: ContextType) t m (), RenderFree c () )
--   -> m ()
-- paintToCanvas ePaint =
--   let
--     applyPaint (cp, ins) =
--       _canvasPaint_paint cp ins
--   in
--     RD.performEvent_ ( applyPaint <$> ePaint )
