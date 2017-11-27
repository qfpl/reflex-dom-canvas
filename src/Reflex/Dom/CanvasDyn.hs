{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
--
{-# LANGUAGE MultiParamTypeClasses #-}

module Reflex.Dom.CanvasDyn
  ( dPaintContext2d
  , dPaintWebgl
  , drawGL
  , drawContext2d
  , drawWithCx
  ) where

import           Control.Lens                    ((^.))
import           Data.Coerce                     (coerce)
import           Data.Proxy                      (Proxy (..))
import           GHC.TypeLits                    (KnownSymbol, symbolVal)

import qualified JSDOM
import           JSDOM.HTMLCanvasElement         (getContextUnchecked)
import           JSDOM.Types                     (IsRenderingContext, JSM,
                                                  MonadJSM,
                                                  RenderingContext (..),
                                                  fromJSValUnchecked, liftJSM,
                                                  toJSVal)

import           Reflex                          (Dynamic, Event, (<@))
import qualified Reflex                          as R

import           Reflex.Dom                      (MonadWidget)
import qualified Reflex.Dom                      as RD

import           Reflex.Dom.CanvasBuilder.Types

dCanvasCx
  :: forall c cx t m. ( MonadWidget t m
                      , KnownSymbol (RenderContextEnum c)
                      , cx ~ RenderContext c
                      , IsRenderingContext cx
                      , HasRenderFn c cx
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

drawCanvasFree
  :: forall t m c cx a. ( MonadWidget t m
                        , cx ~ RenderContext c
                        , IsRenderingContext cx
                        , HasRenderFn c cx
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

drawWithCx
  :: ( MonadWidget t m
     , MonadJSM m
     , IsRenderingContext ( RenderContext c )
     , HasRenderFn c ( RenderContext c )
     )
  => Dynamic t ( RenderContext c )
  -> Dynamic t ( RenderContext c -> Double -> JSM a )
  -> Event t ()
  -> m ( Event t a )
drawWithCx dContext dAction eApply =
  let
    nextFrame f cx = liftJSM $
      JSDOM.nextAnimationFrame (f cx)
  in
    RD.performEvent
    ( nextFrame
      <$> R.current dAction
      <*> R.current dContext
      <@ eApply
    )

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

drawGL
  :: MonadWidget t m
  => Dynamic t ( RenderFree 'Webgl a )
  -> Dynamic t ( RenderContext 'Webgl )
  -> Event t ()
  -> m (Event t a)
drawGL =
  drawCanvasFree

drawContext2d
  :: MonadWidget t m
  => Dynamic t ( RenderFree 'TwoD a )
  -> Dynamic t ( RenderContext 'TwoD )
  -> Event t ()
  -> m (Event t a)
drawContext2d =
  drawCanvasFree
