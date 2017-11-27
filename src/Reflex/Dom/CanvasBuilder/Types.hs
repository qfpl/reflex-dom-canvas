{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
--
{-# LANGUAGE FunctionalDependencies #-}
module Reflex.Dom.CanvasBuilder.Types where

import Data.IORef (IORef)

import           Control.Lens                   (makeLenses)

import           Data.Proxy                     (Proxy (..))
import           GHC.TypeLits                   (Symbol)

import qualified Reflex                         as R
import qualified Reflex.Dom                     as RD

import           JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D (..))

import           JSDOM.Types                    (JSM, WebGLRenderingContext, IsRenderingContext)

import           Data.Text                      (Text)

import           Reflex.Dom.Canvas.WebGL        (WebGLM)
import qualified Reflex.Dom.Canvas.WebGL        as GL

import           Reflex.Dom.Canvas.Context2D           (CanvasM)
import qualified Reflex.Dom.Canvas.Context2D           as TwoD

data ContextType
  = TwoD
  | Webgl

type family RenderContext (a :: ContextType) :: *
type instance RenderContext 'TwoD  = CanvasRenderingContext2D
type instance RenderContext 'Webgl = WebGLRenderingContext

type family RenderContextEnum (a :: ContextType) :: Symbol
type instance RenderContextEnum 'TwoD  = "2d"
type instance RenderContextEnum 'Webgl = "webgl"

type family RenderFree (a :: ContextType) :: * -> *
type instance RenderFree 'TwoD  = CanvasM
type instance RenderFree 'Webgl = WebGLM

data CanvasConfig (c :: ContextType) t = CanvasConfig
  { _canvasConfig_El   :: RD.El t
  , _canvasConfig_Args :: [Text]
  }
makeLenses ''CanvasConfig

data CanvasInfo (c :: ContextType) t = CanvasInfo
  { _canvasInfo_El       :: RD.El t
  , _canvasInfo_context  :: RenderContext c
  , _canvasInfo_keyEvent :: RD.Key -> R.Event t ()
  }
makeLenses ''CanvasInfo

-- Should this be a more useful typeclass that contains the references to
-- the symbols etc so I can avoid the type family shenanigans ?
class IsRenderingContext c ~ IsRenderingContext (RenderContext a) => HasRenderFn a c | a -> c, c -> a where
  renderFunction :: c -> RenderFree a b -> JSM b

instance HasRenderFn 'TwoD CanvasRenderingContext2D where
  renderFunction = flip TwoD.drawToCanvas

instance HasRenderFn 'Webgl WebGLRenderingContext where
  renderFunction = flip GL.drawToCanvas
