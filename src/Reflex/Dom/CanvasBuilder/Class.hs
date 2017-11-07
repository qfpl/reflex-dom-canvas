module Reflex.Dom.CanvasBuilder.Class where

import Reflex.Dom.CanvasBuilder.Types

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
           IsRenderingContext (RenderContext c)
         )
  => RD.MonadAdjust t (ImmediateCanvasBuilderT c t m) where
  runWithReplace (ImmediateCanvasBuilderT a0) a' = do
    r <- ask
    lift
      $ R.runWithReplace (runImmediateCanvasBuilderT ( ImmediateCanvasBuilderT a0 ) r)
      $ fmap (`runImmediateCanvasBuilderT` r) a'

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

  element t c = RD.element t c

  selectElement c = RD.selectElement c

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

runImmediateCanvasBuilderT
  :: ( RD.MonadWidget t m
     , RD.DomBuilderSpace m ~ RD.GhcjsDomSpace
     , IsRenderingContext (RenderContext c)
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
  let canvasCx = _immediateCanvasBuilderEnv_context env

  (cM, as) <- runStateT (runReaderT m env) mempty

  runJSM ( traverse_ (`CanvasF.drawToCanvas` canvasCx) as ) ctx

  pure cM

data CanvasConfig (c :: ContextType) t = CanvasConfig
  { _canvasConfig_El   :: RD.El t
  , _canvasConfig_Args :: [Text]
  }
makeLenses ''CanvasConfig

data CanvasInfo (c :: ContextType) t = CanvasInfo
  { _canvasInfo_keyEvent :: RD.Key -> R.Event t ()
  , _canvasInfo_El       :: RD.El t
  }
makeLenses ''CanvasInfo
