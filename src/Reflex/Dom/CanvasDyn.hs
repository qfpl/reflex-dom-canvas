{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
--
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Main functions for creating your Dynamic canvas element
module Reflex.Dom.CanvasDyn
  ( dContext2d
  , dContextWebgl
  , drawCanvasFree
  , drawWithCx
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

import           Reflex                         (Dynamic, Event, (<@))
import qualified Reflex                         as R

import           Reflex.Dom.Core                (MonadWidget)
import qualified Reflex.Dom.Core                as RD

import           Reflex.Dom.CanvasBuilder.Types

dCanvasCx
  :: forall c t m. ( MonadWidget t m
                   , KnownSymbol (RenderContextEnum c)
                   , IsRenderingContext (RenderContext c)
                   , HasRenderFn c (RenderContext c)
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

-- | Execute instructions from one of the Free Monad implementations of canvas
-- functionality.
drawCanvasFree
  :: ( MonadWidget t m
     , HasRenderFn c ( RenderContext c )
     )
  => Dynamic t ( RenderFree c a )
  -> Dynamic t ( RenderContext c )
  -> Event t ()
  -> m (Event t a)
drawCanvasFree dInstructions dContext eDraw =
  let
    nextFrame cx ins = liftJSM $
      JSDOM.nextAnimationFrame (\_ -> renderFunction cx ins )
  in
    RD.performEvent
    ( nextFrame
      <$> R.current dContext
      <*> R.current dInstructions
      <@  eDraw
    )

-- | Run functions using the raw canvas context object. This will be either a
-- Context2D or ContextWebGL depending on which type you initialised.
drawWithCx
  :: ( MonadWidget t m
     , HasRenderFn c ( RenderContext c )
     )
  => Dynamic t ( RenderContext c )
  -> Dynamic t ( RenderContext c -> Double -> JSM a )
  -> Event t ()
  -> m ( Event t a )
drawWithCx dContext dAction eApply =
  let
    nextFrame cx f = liftJSM $
      JSDOM.nextAnimationFrame (f cx)
  in
    RD.performEvent
    ( nextFrame
      <$> R.current dContext
      <*> R.current dAction
      <@ eApply
    )

-- | Create a canvas for 2D drawing using "context2d".
dContext2d
  :: MonadWidget t m
  => CanvasConfig 'TwoD t
  -> m ( Dynamic t ( CanvasInfo 'TwoD t ) )
dContext2d = dCanvasCx

-- | Create a canvas for use with WebGL.
dContextWebgl
  :: MonadWidget t m
  => CanvasConfig 'Webgl t
  -> m ( Dynamic t ( CanvasInfo 'Webgl t ) )
dContextWebgl = dCanvasCx
