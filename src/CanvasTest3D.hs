{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
--
module CanvasTest3D where

import           Control.Lens                    ((&), (.~))
import           GHC.Int                         (Int32)

import           Reflex.Dom.Canvas.WebGL         (WebGLF)
import qualified Reflex.Dom.Canvas.WebGL         as Gl

import           JSDOM.Types                     (JSString, MonadJSM, castTo,
                                                  liftJSM, toJSValListOf)

import qualified JSDOM.Types                     as Dom

import qualified JSDOM.WebGLRenderingContextBase as Gl

import qualified Reflex.Dom.CanvasBuilder.Types  as Canvas
import qualified Reflex.Dom.CanvasDyn            as CDyn

import           Reflex                          ((<@), (<@>))
import qualified Reflex                          as R

import           Reflex.Dom                      (MonadWidget)
import qualified Reflex.Dom                      as RD

import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Time                       (UTCTime, getCurrentTime)

import           Control.Monad.Except            (ExceptT (..), lift,
                                                  runExceptT)
import           Control.Monad.Free              (MonadFree)
import           Data.Either                     (Either)
import qualified Data.Map                        as Map

import           Data.String                     (fromString)

#ifndef ghcjs_HOST_OS
import qualified Run
#endif

vertShader
  :: Text
vertShader = Text.unlines
  [ "attribute vec4 a_position;"
  , "void main() {"
  , "  gl_Position = a_position;"
  , "}"
  ]

fragShader
  :: Text
fragShader = Text.unlines
  [ "precision mediump float;"
  , "void main() {"
  , "  gl_FragColor = vec4(1, 0, 0.5, 1);"
  , "}"
  ]

positions
  :: [Double]
positions =
  [ 0.1, 0.1
  , 0.1, 0.6
  , 0.8, 0.1
  ]

data RenderMeh = R
  { _rGLProgram  :: Dom.WebGLProgram
  , _rPosAttrLoc :: Dom.GLint
  , _rPosBuffer  :: Dom.WebGLBuffer
  }

glProgramInit
  :: Text
  -> Text
  -> Dom.ArrayBuffer
  -> Gl.WebGLM (Either JSString RenderMeh)
glProgramInit vertSrc fragSrc arrBuffer = runExceptT $ do
  -- Begin initialisation
  vS <- ExceptT $ Gl.buildShader vertSrc Gl.VERTEX_SHADER
  fS <- ExceptT $ Gl.buildShader fragSrc Gl.FRAGMENT_SHADER
  glProg <- ExceptT $ Gl.buildProgram vS fS

  posAttrLoc <- lift $ Gl.getAttribLocationF glProg ( "a_position" :: Text )
  posBuffer <- lift Gl.createBufferF

  lift $ Gl.bindBufferF Gl.ARRAY_BUFFER posBuffer

  lift $ Gl.bufferDataF Gl.ARRAY_BUFFER arrBuffer Gl.STATIC_DRAW
  -- End initialisation
  pure $ R glProg posAttrLoc posBuffer


glProgramDraw
  :: Int32
  -> Int32
  -> RenderMeh
  -> Gl.WebGLM ()
glProgramDraw cW cH (R {..}) = do
  -- Begin Rendering code
  Gl.viewportF 0 0 cW cH

  -- Clear canvas
  Gl.clearColourF 0 0 0 0
  Gl.clearF Gl.COLOR_BUFFER_BIT

  -- Tell WebGL to use our pair of shaders
  Gl.useProgramF _rGLProgram

  -- Buffer Setup and Loading
  Gl.enableVertexAttribArrayF ( fromIntegral _rPosAttrLoc )

  Gl.bindBufferF Gl.ARRAY_BUFFER _rPosBuffer

  let
    size      = 2               -- 2 components per iteration
    dataType  = Gl.FLOAT        -- the data is 32bit floats
    normalise = False           -- don't normalize the data
    stride    = size * 4 -- move forward size * sizeof(type) each iteration to get the next position
    offset    = 0        -- start at the beginning of the buffer

  -- Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
  Gl.vertexAttribPointerF ( fromIntegral _rPosAttrLoc ) size dataType normalise stride offset

  let
    primitiveType = Gl.TRIANGLES
    offset' = 0
    count = 3

  Gl.drawArraysF primitiveType offset' count

eDraw :: MonadWidget t m => UTCTime -> m ()
eDraw aTime = do
  let
    canvasH = 480
    canvasW = 640

    canvasId = "canvas-three-dee"
    canvasAttrs = pure $ Map.fromList
      [ ("height", "480")
      , ("width", "640")
      ]

  eInit <- RD.button "Init"
  eRender <- RD.button "Render"

  -- Create some JS Data
  -- Need a better way of mangling this... add to util Free and Coproduct to victory?
  arrBuffer <- liftJSM (Dom.uncheckedCastTo Dom.ArrayBuffer <$> toJSValListOf positions)

  -- Create the canvas element
  canvasEl <- fst <$> RD.elDynAttr' "canvas"
    (Map.insert "id" canvasId <$> canvasAttrs) RD.blank

  -- Provide polymorphic painter here...
  dGLCX <- CDyn.dGLCx ( Canvas.CanvasConfig canvasEl [] )

  let
    glInitProg = CDyn.drawGL
      $ glProgramInit vertShader fragShader arrBuffer

  (eInitFailed, eRenderMeh) <-
    R.fanEither <$> glInitProg (R.current dGLCX <@ eInit)

  dDrawing <- R.holdDyn Gl.noopF $
    -- Only render noops until we have successfully initialised
    glProgramDraw canvasW canvasH <$> eRenderMeh

  dStatus <- R.holdDyn "A little nothing..." eInitFailed

  let
    dAction =
      (\instructions -> CDyn.drawGL instructions (R.current dGLCX <@ eRender ) ) <$> dDrawing

  eDrew <- RD.dyn dAction

  RD.divClass "errorz" $
    RD.display dStatus

  pure ()

#ifdef ghcjs_HOST_OS
mainish
  :: IO ()
mainish = do
  n <- getCurrentTime
  RD.mainWidget ( eDraw n )
#endif

#ifndef ghcjs_HOST_OS
mainish
  :: IO ()
mainish = do
  n <- getCurrentTime
  Run.run ( eDraw n )
#endif
