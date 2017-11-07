{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PolyKinds #-}
-- Sigh, would like to get rid of need for Monad*Control if possible.
-- Not convinced I need it, added to meet reqs, for now.
{-# LANGUAGE UndecidableInstances       #-}

{-# LANGUAGE FunctionalDependencies #-}
module Reflex.Dom.CanvasBuilder
  ( Canvas2DM
  , CanvasWebGLM
  , Monad2DCanvas
  , MonadGLCanvas
  , CanvasConfig (..)
  , CanvasInfo (..)
  , ContextType (..)
  , canvasInfo_El
  , canvasInfo_keyEvent
  , canvasConfig_Args
  , canvasConfig_El
  , with2DContext
  , withWebGLContext
  , liftCx
  -- Experiments !
  , liftCx2d
  ) where

import           GHC.IORef                      (IORef)

import           Data.Coerce                    (coerce)

import           Control.Lens                   (makeLenses, snoc, (^.), _1, to)

#if MIN_VERSION_base(4,9,1)
import           Control.Monad.Exception        (MonadAsyncException,
                                                 MonadException)
#else
import           Control.Monad.Exception        (MonadException)
#endif

import           Control.Monad                  ((<=<))
import           Control.Monad.Primitive        (PrimMonad (..))
import           Control.Monad.Ref              (MonadAtomicRef (..),
                                                 MonadRef (..), Ref)

import           Control.Monad.Fix              (MonadFix)
import           Control.Monad.IO.Class         (MonadIO)

import           Control.Monad.Trans.Control    (MonadTransControl (..),
                                                 defaultLiftWith2,
                                                 defaultRestoreT2)

import           Control.Monad.State            (MonadState, StateT (..),
                                                 evalStateT, modify)

import           Control.Monad.Reader           (MonadReader, MonadTrans,
                                                 ReaderT (..), ask, asks, lift)
import           Control.Monad.Trans.Maybe      (MaybeT (..))

import           Data.Foldable                  (traverse_)
import           Data.Text                      (Text)

import           JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D (..))
import           JSDOM.HTMLCanvasElement        (HTMLCanvasElement)
import qualified JSDOM.HTMLCanvasElement        as HTMLCanvas

import           JSDOM.Types                    (IsRenderingContext, JSM,
                                                 MonadJSM,
                                                 RenderingContext (..),
                                                 WebGLRenderingContext, fromJSValUnchecked,
                                                 fromJSVal, liftJSM, runJSM,
                                                 toJSVal, withCallback)

import qualified JSDOM

import           Data.Sequence                  (Seq)

import qualified Reflex                         as R
import qualified Reflex.Dom                     as RD
import qualified Reflex.Host.Class              as R

import           Reflex.Dom.CanvasBuilder.Types

import           Reflex.Dom.Canvas2DF           (CanvasM)
import qualified Reflex.Dom.Canvas2DF           as CanvasF

import           Data.Proxy                     (Proxy (..))
import           GHC.TypeLits                   (KnownSymbol, symbolVal)

#ifndef ghcjs_HOST_OS
import           GHCJS.DOM.Types                (MonadJSM (..))

instance ( IsRenderingContext (RenderContext c), MonadJSM m ) => MonadJSM (ImmediateCanvasBuilderT c t m) where
  liftJSM' = ImmediateCanvasBuilderT . liftJSM'
#endif

newtype ImmediateCanvasBuilderT (c :: ContextType) t m a = ImmediateCanvasBuilderT
  { _unImmediateDomBuilderT :: ReaderT (ImmediateCanvasBuilderEnv c t) (StateT Actions m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader ( ImmediateCanvasBuilderEnv c t )
           , MonadState Actions
           , MonadFix
           , MonadIO
           , MonadException
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

instance R.PerformEvent t m => R.PerformEvent t (ImmediateCanvasBuilderT (c :: ContextType) t m) where
  type Performable (ImmediateCanvasBuilderT (c :: ContextType) t m) = RD.Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ R.performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ R.performEvent e

instance R.PostBuild t m => R.PostBuild t (ImmediateCanvasBuilderT (c :: ContextType) t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift RD.getPostBuild

instance R.MonadReflexCreateTrigger t m => R.MonadReflexCreateTrigger t (ImmediateCanvasBuilderT (c :: ContextType) t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . R.newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ R.newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, R.MonadReflexCreateTrigger t m, R.TriggerEvent t m)
  => R.TriggerEvent t (ImmediateCanvasBuilderT (c :: ContextType) t m) where

  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = ImmediateCanvasBuilderT . lift $ R.newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = ImmediateCanvasBuilderT . lift $ R.newTriggerEventWithOnComplete
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = ImmediateCanvasBuilderT . lift $ R.newEventWithLazyTriggerWithOnComplete f

instance RD.HasJSContext m => RD.HasJSContext (ImmediateCanvasBuilderT (c :: ContextType) t m) where
  type JSContextPhantom (ImmediateCanvasBuilderT (c :: ContextType) t m) = RD.JSContextPhantom m
  askJSContext = lift RD.askJSContext

instance RD.HasDocument m => RD.HasDocument (ImmediateCanvasBuilderT c t m)

instance RD.MonadSample t m => RD.MonadSample t (ImmediateCanvasBuilderT c t m) where
  {-# INLINABLE sample #-}
  sample = lift . RD.sample

instance RD.MonadHold t m => RD.MonadHold t (ImmediateCanvasBuilderT c t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ RD.hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ RD.holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ RD.holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic p e = lift $ RD.buildDynamic p e
  {-# INLINABLE headE #-}
  headE e = lift $ RD.headE e

instance MonadRef m => MonadRef (ImmediateCanvasBuilderT (c :: ContextType) t m) where
  type Ref (ImmediateCanvasBuilderT (c :: ContextType) t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (ImmediateCanvasBuilderT (c :: ContextType) t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance ( R.Reflex t,
           RD.MonadAdjust t m,
           RD.MonadWidget t m,
           MonadJSM m,
           RD.MonadHold t m,
           MonadFix m,
           PrimMonad m,
           HasRenderFn c,
           IsRenderingContext (RenderContext c)
         )
  => RD.MonadAdjust t (ImmediateCanvasBuilderT c t m) where
  runWithReplace (ImmediateCanvasBuilderT a0) a' = do
    r <- ask
    lift
      $ R.runWithReplace (runImmediateCanvasBuilderT ( ImmediateCanvasBuilderT a0 ) r)
      $ fmap (\a -> runImmediateCanvasBuilderT a r) a'

  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift
      $ R.traverseIntMapWithKeyWithAdjust (\k v -> runImmediateCanvasBuilderT (f k v) r) dm0 dm'

  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift
      $ R.traverseDMapWithKeyWithAdjust (\k v -> runImmediateCanvasBuilderT (f k v) r) dm0 dm'

  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- ask
    lift $ R.traverseDMapWithKeyWithAdjustWithMove (\k v -> runImmediateCanvasBuilderT (f k v) r) dm0 dm'

instance PrimMonad m => PrimMonad (ImmediateCanvasBuilderT c t m) where
  type PrimState (ImmediateCanvasBuilderT c t m) = PrimState m
  primitive = lift . primitive

instance MonadTrans (ImmediateCanvasBuilderT c t) where
  lift = ImmediateCanvasBuilderT . lift . lift

-- instance MonadTransControl (ImmediateCanvasBuilderT c t) where
--   type StT (ImmediateCanvasBuilderT c t) a = a
--   liftWith = defaultLiftWith ImmediateCanvasBuilderT _unImmediateDomBuilderT
--   restoreT = defaultRestoreT ImmediateCanvasBuilderT

instance MonadTransControl (ImmediateCanvasBuilderT c t) where
  type StT ( ImmediateCanvasBuilderT c t ) a = StT (StateT Actions) (StT ( ReaderT ( ImmediateCanvasBuilderEnv c t ) ) a)
  liftWith = defaultLiftWith2 ImmediateCanvasBuilderT _unImmediateDomBuilderT
  restoreT = defaultRestoreT2 ImmediateCanvasBuilderT

instance ( RD.SupportsImmediateDomBuilder t m
         , IsRenderingContext (RenderContext c)
         , HasRenderFn c
         , RD.DomBuilderSpace m ~ RD.GhcjsDomSpace
         , Ref (RD.Performable m) ~ IORef
         , RD.DomBuilder t m
         , RD.PostBuild t m
         , RD.PerformEvent t m
         , RD.TriggerEvent t m
         , RD.HasJSContext m
         , RD.HasJSContext (RD.Performable m)
         , RD.HasDocument m
         , MonadRef (RD.Performable m)
         , MonadJSM (RD.Performable m)
         , RD.MonadSample t (RD.Performable m)
         ) => RD.DomBuilder t (ImmediateCanvasBuilderT c t m) where
  type DomBuilderSpace (ImmediateCanvasBuilderT c t m) = RD.GhcjsDomSpace

  {-# INLINABLE element #-}
  element = RD.element
  {-# INLINABLE selectElement #-}
  selectElement = RD.selectElement

type MonadCanvasConstraints (c :: ContextType) t m =
  ( RD.MonadWidget t m
  , IsRenderingContext (RenderContext c)
  , MonadReader ( ImmediateCanvasBuilderEnv c t ) m
  , MonadState Actions m
  )

type MonadGLCanvas t m = MonadCanvasConstraints 'Webgl t m
type Monad2DCanvas t m = MonadCanvasConstraints 'TwoD t m

type Canvas2DM t m a    = ImmediateCanvasBuilderT 'TwoD t m a
type CanvasWebGLM t m a = ImmediateCanvasBuilderT 'Webgl t m a

class HasRenderFn a where
  renderFunction :: Proxy a -> RenderContext a -> CanvasM () -> JSM ()

instance HasRenderFn 'TwoD where
  renderFunction _ = flip CanvasF.drawToCanvas

instance HasRenderFn 'Webgl where
  renderFunction _ = error "webgl render function not implemented"

runImmediateCanvasBuilderT
  :: forall c t m a. ( RD.MonadWidget t m
                     , RD.DomBuilderSpace m ~ RD.GhcjsDomSpace
                     , IsRenderingContext (RenderContext c)
                     , HasRenderFn c
                     , RD.PerformEvent t m
                     , MonadJSM m
                     , MonadJSM (RD.Performable m)
                     , MonadRef m
                     , Ref m ~ IORef
                     )
  => ImmediateCanvasBuilderT c t m a
  -> ImmediateCanvasBuilderEnv c t
  -> m a
runImmediateCanvasBuilderT (ImmediateCanvasBuilderT m) env = do
  ctx <- RD.unJSContextSingleton <$> RD.askJSContext
  let
    renderFn = renderFunction (Proxy :: Proxy c)
      $ _immediateCanvasBuilderEnv_context env

    paintCanvas paints = runJSM
      ( JSDOM.nextAnimationFrame (\_ -> traverse_ renderFn paints ) )
      ctx

  (a, cM) <- runStateT (runReaderT m env) mempty

  -- RD.dyn ( paintCanvas <$> cM )

  paintCanvas cM

  pure a

withSomeContext
  :: forall c t m. ( RD.MonadWidget t m
                   , KnownSymbol (RenderContextEnum c)
                   , IsRenderingContext (RenderContext c)
                   , HasRenderFn c
                   )
  => CanvasConfig c t
  -> ImmediateCanvasBuilderT c t m ()
  -> m (CanvasInfo c t)
withSomeContext cfg canvasActions = do
  let
    reflexEl = cfg ^. canvasConfig_El
    cxType   = symbolVal ( Proxy :: Proxy (RenderContextEnum c) )

    drawFunction ( dele, cx ) = runImmediateCanvasBuilderT canvasActions
      $ ImmediateCanvasBuilderEnv dele ( coerce cx )

  htmlCanvas <- liftJSM $ fromJSValUnchecked =<< toJSVal ( RD._element_raw reflexEl )
  canvasCx <- liftJSM $ HTMLCanvas.getContextUnchecked htmlCanvas cxType (cfg ^. canvasConfig_Args)

  drawFunction (htmlCanvas, canvasCx)
  -- ( traverse_ drawFunction =<< ) . runMaybeT $ do
  --   e <- MaybeT . liftJSM $ fromJSVal =<< toJSVal ( RD._element_raw reflexEl )
  --   c <- MaybeT . liftJSM $ HTMLCanvas.getContext e cxType (cfg ^. canvasConfig_Args)
  --   pure (e,c)

  pure $ CanvasInfo (`RD.keypress` reflexEl) reflexEl

with2DContext
  :: RD.MonadWidget t m
  => CanvasConfig 'TwoD t
  -> Canvas2DM t m ()
  -> m (CanvasInfo 'TwoD t)
with2DContext =
  withSomeContext

withWebGLContext
  :: RD.MonadWidget t m
  => CanvasConfig 'Webgl t
  -> CanvasWebGLM t m ()
  -> m (CanvasInfo 'Webgl t)
withWebGLContext =
  withSomeContext

liftCx2d
  :: ( MonadCanvasConstraints c t m
     , IsRenderingContext cx ~ IsRenderingContext (RenderContext c)
     )
  => CanvasM ()
  -> m ()
liftCx2d = modify
  -- . fmap
  . flip snoc

liftCx
  :: ( MonadCanvasConstraints c t m
     , IsRenderingContext cx ~ IsRenderingContext (RenderContext c)
     )
  => (cx -> JSM a)
  -> m a
liftCx f = do
  ctx <- RD.unJSContextSingleton <$> RD.askJSContext
  canvasCx <- asks _immediateCanvasBuilderEnv_context
  runJSM (f canvasCx) ctx

-- | Example usage of liftCx
-- createShaderM
--   :: MonadGLCanvas t m
--   => GLenum
--   -> m WebGLShader
-- createShaderM shaderType =
--   liftCx (`WebGL.createShader` shaderType)
--
-- beginPathM
--   :: Monad2DCanvas t m
--   => m ()
-- beginPathM =
--   liftCx beginPath
