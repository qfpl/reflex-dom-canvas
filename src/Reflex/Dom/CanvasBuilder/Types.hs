{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Dom.CanvasBuilder.Types where

import           Control.Lens                   (makeLenses)

import Data.Proxy (Proxy (..))
import           GHC.TypeLits                   (Symbol)

import           Reflex (Dynamic)
import qualified Reflex                         as R
import qualified Reflex.Dom                     as RD

import           JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D (..))
import           JSDOM.HTMLCanvasElement        (HTMLCanvasElement)

import           JSDOM.Types                    (WebGLRenderingContext, JSM)

import           Data.Sequence                  (Seq)
import           Data.Text                      (Text)

import           Reflex.Dom.Canvas2DF           (CanvasM,drawToCanvas)

data ContextType
  = TwoD
  | Webgl

type family RenderContext (a :: ContextType) :: *
type instance RenderContext 'TwoD  = CanvasRenderingContext2D
type instance RenderContext 'Webgl = WebGLRenderingContext

type family RenderContextEnum (a :: ContextType) :: Symbol
type instance RenderContextEnum 'TwoD  = "2d"
type instance RenderContextEnum 'Webgl = "webgl"

data ImmediateCanvasBuilderEnv (c :: ContextType) t = ImmediateCanvasBuilderEnv
  { _immediateCanvasBuilderEnv_element :: {-# UNPACK #-} !HTMLCanvasElement
  , _immediateCanvasBuilderEnv_context :: RenderContext c
  , _immediateCanvasBuilderEnv_dynamic :: Dynamic t ( Seq (CanvasM ()) )
  }
makeLenses ''ImmediateCanvasBuilderEnv

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

data CanvasPaint (c :: ContextType) t m = CanvasPaint
  { _canvasPaint_paint   :: RD.MonadWidget t m => CanvasM () -> m ()
  , _canvasPaint_actions :: CanvasM ()
  , _canvasPaint_keyEvent :: RD.Key -> R.Event t ()
  }
makeLenses ''CanvasPaint

class HasRenderFn a where
  renderFunction :: Proxy a -> RenderContext a -> CanvasM () -> JSM ()

instance HasRenderFn 'TwoD where
  renderFunction _ =
    flip drawToCanvas

instance HasRenderFn 'Webgl where
  renderFunction _ =
    error "webgl render function not implemented"
