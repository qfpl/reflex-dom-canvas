{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Dom.Canvas.WebGL where

import           JSDOM.Types               (BufferSource, GLboolean, GLclampf,
                                            GLenum, GLfloat, GLint, GLintptr,
                                            GLsizei, GLuint, IsBufferSource,
                                            JSString, JSVal, ToJSString (..),
                                            WebGLBuffer, WebGLProgram, MonadJSM,
                                            WebGLShader, WebGLUniformLocation,
                                            toBufferSource)

import qualified JSDOM.WebGLRenderingContextBase as W

import           Control.Monad.Free.Church

-- my my, aren't shaders and programs similar...PREPARE THE WIFFLE BAT OF TYPE SAFETY.
data WebGLF a
  -- Shader Mangling
  = CreateShader GLenum a
  | ShaderSource WebGLShader JSString a
  | CompileShader WebGLShader a
  | GetShaderParameter WebGLShader GLenum (JSVal -> a)
  | GetShaderInfoLog WebGLShader (Maybe JSString -> a)
  | DeleteShader WebGLShader a

  -- Program Mangling
  | CreateProgram a
  | AttachShader WebGLProgram WebGLShader a
  | LinkProgram WebGLProgram a
  | GetProgramParameter WebGLProgram GLenum (JSVal -> a)
  | GetProgramInfoLog WebGLProgram (Maybe JSString -> a)
  | DeleteProgram WebGLProgram a
  | UseProgram WebGLProgram a

  -- Attribbles
  | GetAttribLocation WebGLProgram JSString (GLint -> a)
  | EnableVertexAttribArray GLuint a
  | VertexAttribPointer GLuint GLint GLenum GLboolean GLsizei GLintptr a

  -- Uniforms
  | GetUniformLocation WebGLProgram JSString (WebGLUniformLocation -> a)
  | Uniform2f WebGLUniformLocation GLfloat GLfloat a
  | Uniform4f WebGLUniformLocation GLfloat GLfloat GLfloat GLfloat a

  -- Buffers
  | CreateBuffer (WebGLBuffer -> a)
  | BindBuffer GLenum WebGLBuffer a
  | BufferData GLenum BufferSource GLenum a

  -- Viewport
  | Viewport GLint GLint GLsizei GLsizei a

  -- Colour
  | ClearColour GLclampf GLclampf GLclampf GLclampf a
  | Clear GLenum a

  -- Get Busy
  | DrawArrays GLenum GLint GLsizei a
  deriving Functor

type WebGLM = F WebGLF

liftF' :: MonadFree WebGLF m => ((a -> a) -> WebGLF a) -> m a
liftF' = liftF . ($ id)

liftF_ :: MonadFree WebGLF m => (() -> WebGLF ()) -> m ()
liftF_ = liftF . ($ ())

-- Get Busy

drawArraysF :: MonadFree WebGLF m => GLenum -> GLint -> GLsizei -> m ()
drawArraysF gle i1 = liftF_ . DrawArrays gle i1

-- Colour

clearColourF :: MonadFree WebGLF m => GLclampf -> GLclampf -> GLclampf -> GLclampf -> m ()
clearColourF c1 c2 c3 = liftF_ . ClearColour c1 c2 c3

clearF :: MonadFree WebGLF m => GLenum -> m ()
clearF = liftF_ . Clear

-- Viewport

viewportF :: MonadFree WebGLF m => GLint -> GLint -> GLsizei -> GLsizei -> m ()
viewportF i1 i2 s1 = liftF_ . Viewport i1 i2 s1

-- Buffers

createBufferF :: MonadFree WebGLF m => m WebGLBuffer
createBufferF = liftF' CreateBuffer

bindBufferF :: MonadFree WebGLF m => GLenum -> WebGLBuffer -> m ()
bindBufferF gle = liftF_ . BindBuffer gle

bufferDataF
  :: ( MonadFree WebGLF m
     , IsBufferSource bufferSource
     )
  => GLenum
  -> bufferSource
  -> GLenum
  -> m ()
bufferDataF gle1 buff =
  liftF_ . BufferData gle1 ( toBufferSource buff )

-- Uniforms

getUniformLocationF
  :: ( MonadFree WebGLF m
     , ToJSString location
     )
  => WebGLProgram
  -> location
  -> m WebGLUniformLocation
getUniformLocationF p =
  liftF' . GetUniformLocation p . toJSString

uniform2fF
  :: MonadFree WebGLF m
  => WebGLUniformLocation
  -> GLfloat
  -> GLfloat
  -> m ()
uniform2fF location v1 =
  liftF_ . Uniform2f location v1

uniform4fF
  :: MonadFree WebGLF m
  => WebGLUniformLocation
  -> GLfloat
  -> GLfloat
  -> GLfloat
  -> GLfloat
  -> m ()
uniform4fF location v1 v2 v3 =
  liftF_ . Uniform4f location v1 v2 v3

-- Attribbles
getAttribLocationF
  :: ( MonadFree WebGLF m
     , ToJSString location
     )
  => WebGLProgram
  -> location
  -> m GLint
getAttribLocationF p =
  liftF' . GetAttribLocation p . toJSString

enableVertexAttribArrayF :: MonadFree WebGLF m => GLuint -> m ()
enableVertexAttribArrayF = liftF_ . EnableVertexAttribArray

vertexAttribPointerF
  :: MonadFree WebGLF m
  => GLuint
  -> GLint
  -> GLenum
  -> GLboolean
  -> GLsizei
  -> GLintptr
  -> m ()
vertexAttribPointerF index size type' normalized stride =
  liftF_ . VertexAttribPointer index size type' normalized stride

-- Programs
createProgramF :: MonadFree WebGLF m => m ()
createProgramF = liftF_ CreateProgram

attachShaderF :: MonadFree WebGLF m => WebGLProgram -> WebGLShader -> m ()
attachShaderF p = liftF_ . AttachShader p

linkProgramF :: MonadFree WebGLF m => WebGLProgram -> m ()
linkProgramF = liftF_ . LinkProgram

getProgramParameterF :: MonadFree WebGLF m => WebGLProgram -> GLenum -> m JSVal
getProgramParameterF p = liftF' . GetProgramParameter p

getProgramInfoLogF :: MonadFree WebGLF m => WebGLProgram -> m (Maybe JSString)
getProgramInfoLogF = liftF' . GetProgramInfoLog

deleteProgramF :: MonadFree WebGLF m => WebGLProgram -> m ()
deleteProgramF = liftF_ . DeleteProgram

useProgramF :: MonadFree WebGLF m => WebGLProgram -> m ()
useProgramF = liftF_ . UseProgram

-- Shaders
createShaderF :: MonadFree WebGLF m => GLenum -> m ()
createShaderF = liftF_ . CreateShader

shaderSourceF :: ( MonadFree WebGLF m, ToJSString string ) => WebGLShader -> string -> m ()
shaderSourceF shdr = liftF_ . ShaderSource shdr . toJSString

compileShaderF :: MonadFree WebGLF m => WebGLShader -> m ()
compileShaderF = liftF_ . CompileShader

getShaderParameterF :: MonadFree WebGLF m => WebGLShader -> GLenum -> m JSVal
getShaderParameterF s = liftF' . GetShaderParameter s

getShaderInfoLogF :: MonadFree WebGLF m => WebGLShader -> m (Maybe JSString)
getShaderInfoLogF = liftF' . GetShaderInfoLog

deleteShaderF :: MonadFree WebGLF m => WebGLShader -> m ()
deleteShaderF = liftF_ . DeleteShader

-- data CoWebGLF k = CoWebGLF
--   { createShader            :: GLenum -> k
--   , shaderSource            :: WebGLShader -> JSString -> k
--   , compileShader           :: WebGLShader -> k
--   , getShaderParameter      :: WebGLShader -> GLenum -> (JSVal, k)
--   , getShaderLogInfo        :: WebGLShader -> (Maybe JSString, k)
--   , deleteShader            :: WebGLShader -> k
--   -- Programs
--   , createProgram           :: k
--   , attachShader            :: WebGLProgram -> WebGLShader -> k
--   , linkProgram             :: WebGLProgram -> k
--   , getProgramParameter     :: WebGLProgram -> GLenum -> (JSVal, k)
--   , getProgramInfoLog       :: WebGLProgram -> (Maybe JSString, k)
--   , deleteProgram           :: WebGLProgram -> k
--   , useProgram              :: WebGLProgram -> k
--   , getAttribLocation       :: WebGLProgram -> JSString -> (GLint, k)
--   , enableVertexAttribArray :: GLuint -> k
--   , vertexAttribPointer     :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> k
--   -- Uniforms
--   , getUniformLocation      :: WebGLProgram -> JSString -> (WebGLUniformLocation, k)
--   , uniform2f               :: WebGLUniformLocation -> GLfloat -> GLfloat -> k
--   , uniform4f               :: WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> k
--   -- Buffers
--   , createBuffer            :: WebGLBuffer -> k
--   , bindBuffer              :: GLenum -> WebGLBuffer -> k
--   , bufferData              :: GLenum -> BufferSource -> GLenum -> k
--   -- Viewport
--   , viewport                :: GLint -> GLint -> GLsizei -> GLsizei -> k
--   -- Colour
--   , clearColour             :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> k
--   , clear                   :: GLenum -> k
--   -- Get Busy
--   , drawArrays              :: GLenum -> GLint -> GLsizei -> k
--   }
--   deriving Functor


