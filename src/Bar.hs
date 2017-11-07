module Bar where

saveF :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
saveF self = liftDOM (void (self ^. jsf "save" ()))

restoreF :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
restoreF self = liftDOM (void (self ^. jsf "restore" ()))

commitF :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
commitF self = liftDOM (void (self ^. jsf "commit" ()))

scaleF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> m ()
scaleF self sx sy
  = liftDOM (void (self ^. jsf "scale" [toJSVal sx, toJSVal sy]))

rotateF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
rotateF self angle
  = liftDOM (void (self ^. jsf "rotate" [toJSVal angle]))

translateF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> m ()
translateF self tx ty
  = liftDOM (void (self ^. jsf "translate" [toJSVal tx, toJSVal ty]))

transformF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
transformF self m11 m12 m21 m22 dx dy
  = liftDOM
      (void
         (self ^. jsf "transform"
            [toJSVal m11, toJSVal m12, toJSVal m21, toJSVal m22, toJSVal dx,
             toJSVal dy]))

setTransformF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
setTransformF self m11 m12 m21 m22 dx dy
  = liftDOM
      (void
         (self ^. jsf "setTransform"
            [toJSVal m11, toJSVal m12, toJSVal m21, toJSVal m22, toJSVal dx,
             toJSVal dy]))

resetTransformF :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
resetTransformF self
  = liftDOM (void (self ^. jsf "resetTransform" ()))

setLineDashF :: (MonadDOM m) => CanvasRenderingContext2D -> [Float] -> m ()
setLineDashF self dash
  = liftDOM (void (self ^. jsf "setLineDash" [toJSVal (array dash)]))

getLineDashF :: (MonadDOM m) => CanvasRenderingContext2D -> m [Float]
getLineDashF self
  = liftDOM ((self ^. jsf "getLineDash" ()) >>= fromJSArrayUnchecked)

getLineDash_F :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
getLineDash_F self = liftDOM (void (self ^. jsf "getLineDash" ()))

clearRectF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
clearRectF self x y width height
  = liftDOM
      (void
         (self ^. jsf "clearRect"
            [toJSVal x, toJSVal y, toJSVal width, toJSVal height]))

fillRectF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
fillRectF self x y width height
  = liftDOM
      (void
         (self ^. jsf "fillRect"
            [toJSVal x, toJSVal y, toJSVal width, toJSVal height]))

beginPathF :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
beginPathF self = liftDOM (void (self ^. jsf "beginPath" ()))

fillPathF :: (MonadDOM m) => CanvasRenderingContext2D -> Path2D -> Maybe CanvasWindingRule -> m ()
fillPathF self path winding
  = liftDOM
      (void (self ^. jsf "fill" [toJSVal path, toJSVal winding]))

strokePathF :: (MonadDOM m) => CanvasRenderingContext2D -> Path2D -> m ()
strokePathF self path
  = liftDOM (void (self ^. jsf "stroke" [toJSVal path]))

clipPathF :: (MonadDOM m) => CanvasRenderingContext2D -> Path2D -> Maybe CanvasWindingRule -> m ()
clipPathF self path winding
  = liftDOM
      (void (self ^. jsf "clip" [toJSVal path, toJSVal winding]))

fillF :: (MonadDOM m) => CanvasRenderingContext2D -> Maybe CanvasWindingRule -> m ()
fillF self winding
  = liftDOM (void (self ^. jsf "fill" [toJSVal winding]))

strokeF :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
strokeF self = liftDOM (void (self ^. jsf "stroke" ()))

clipF :: (MonadDOM m) => CanvasRenderingContext2D -> Maybe CanvasWindingRule -> m ()
clipF self winding
  = liftDOM (void (self ^. jsf "clip" [toJSVal winding]))

isPointInPathPathF :: (MonadDOM m) => CanvasRenderingContext2D -> Path2D -> Float -> Float -> Maybe CanvasWindingRule -> m Bool
isPointInPathPathF self path x y winding
  = liftDOM
      ((self ^. jsf "isPointInPath"
          [toJSVal path, toJSVal x, toJSVal y, toJSVal winding])
         >>= valToBool)

isPointInPathPath_F :: (MonadDOM m) => CanvasRenderingContext2D -> Path2D -> Float -> Float -> Maybe CanvasWindingRule -> m ()
isPointInPathPath_F self path x y winding
  = liftDOM
      (void
         (self ^. jsf "isPointInPath"
            [toJSVal path, toJSVal x, toJSVal y, toJSVal winding]))

isPointInStrokePathF :: (MonadDOM m) => CanvasRenderingContext2D -> Path2D -> Float -> Float -> m Bool
isPointInStrokePathF self path x y
  = liftDOM
      ((self ^. jsf "isPointInStroke"
          [toJSVal path, toJSVal x, toJSVal y])
         >>= valToBool)

isPointInStrokePath_F :: (MonadDOM m) => CanvasRenderingContext2D -> Path2D -> Float -> Float -> m ()
isPointInStrokePath_F self path x y
  = liftDOM
      (void
         (self ^. jsf "isPointInStroke"
            [toJSVal path, toJSVal x, toJSVal y]))

isPointInPathF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Maybe CanvasWindingRule -> m Bool
isPointInPathF self x y winding
  = liftDOM
      ((self ^. jsf "isPointInPath"
          [toJSVal x, toJSVal y, toJSVal winding])
         >>= valToBool)

isPointInPath_F :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Maybe CanvasWindingRule -> m ()
isPointInPath_F self x y winding
  = liftDOM
      (void
         (self ^. jsf "isPointInPath"
            [toJSVal x, toJSVal y, toJSVal winding]))

isPointInStrokeF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> m Bool
isPointInStrokeF self x y
  = liftDOM
      ((self ^. jsf "isPointInStroke" [toJSVal x, toJSVal y]) >>=
         valToBool)

isPointInStroke_F :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> m ()
isPointInStroke_F self x y
  = liftDOM
      (void (self ^. jsf "isPointInStroke" [toJSVal x, toJSVal y]))

measureTextF :: (MonadDOM m, ToJSString text) => CanvasRenderingContext2D -> text -> m TextMetrics
measureTextF self text
  = liftDOM
      ((self ^. jsf "measureText" [toJSVal text]) >>= fromJSValUnchecked)

measureText_F :: (MonadDOM m, ToJSString text) => CanvasRenderingContext2D -> text -> m ()
measureText_F self text
  = liftDOM (void (self ^. jsf "measureText" [toJSVal text]))

setAlphaF :: (MonadDOM m) => CanvasRenderingContext2D -> Maybe Float -> m ()
setAlphaF self alpha
  = liftDOM (void (self ^. jsf "setAlpha" [toJSVal alpha]))

setCompositeOperationF :: (MonadDOM m, ToJSString compositeOperation) => CanvasRenderingContext2D -> Maybe compositeOperation -> m ()
setCompositeOperationF self compositeOperation
  = liftDOM
      (void
         (self ^. jsf "setCompositeOperation" [toJSVal compositeOperation]))

setLineWidthFunctionF :: (MonadDOM m) => CanvasRenderingContext2D -> Maybe Float -> m ()
setLineWidthFunctionF self width
  = liftDOM (void (self ^. jsf "setLineWidth" [toJSVal width]))

setLineCapFunctionF :: (MonadDOM m, ToJSString cap) => CanvasRenderingContext2D -> Maybe cap -> m ()
setLineCapFunctionF self cap
  = liftDOM (void (self ^. jsf "setLineCap" [toJSVal cap]))

setLineJoinFunctionF :: (MonadDOM m, ToJSString join) => CanvasRenderingContext2D -> Maybe join -> m ()
setLineJoinFunctionF self join
  = liftDOM (void (self ^. jsf "setLineJoin" [toJSVal join]))

setMiterLimitFunctionF :: (MonadDOM m) => CanvasRenderingContext2D -> Maybe Float -> m ()
setMiterLimitFunctionF self limit
  = liftDOM (void (self ^. jsf "setMiterLimit" [toJSVal limit]))

clearShadowF :: (MonadDOM m) => CanvasRenderingContext2D -> m ()
clearShadowF self = liftDOM (void (self ^. jsf "clearShadow" ()))

fillTextF :: (MonadDOM m, ToJSString text) => CanvasRenderingContext2D -> text -> Float -> Float -> Maybe Float -> m ()
fillTextF self text x y maxWidth
  = liftDOM
      (void
         (self ^. jsf "fillText"
            [toJSVal text, toJSVal x, toJSVal y, toJSVal maxWidth]))

strokeTextF :: (MonadDOM m, ToJSString text) => CanvasRenderingContext2D -> text -> Float -> Float -> Maybe Float -> m ()
strokeTextF self text x y maxWidth
  = liftDOM
      (void
         (self ^. jsf "strokeText"
            [toJSVal text, toJSVal x, toJSVal y, toJSVal maxWidth]))

setStrokeColorF :: (MonadDOM m, ToJSString color) => CanvasRenderingContext2D -> color -> Maybe Float -> m ()
setStrokeColorF self color alpha
  = liftDOM
      (void
         (self ^. jsf "setStrokeColor" [toJSVal color, toJSVal alpha]))

setStrokeColorGrayF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Maybe Float -> m ()
setStrokeColorGrayF self grayLevel alpha
  = liftDOM
      (void
         (self ^. jsf "setStrokeColor" [toJSVal grayLevel, toJSVal alpha]))

setStrokeColorRGBF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
setStrokeColorRGBF self r g b a
  = liftDOM
      (void
         (self ^. jsf "setStrokeColor"
            [toJSVal r, toJSVal g, toJSVal b, toJSVal a]))

setStrokeColorCYMKF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> m ()
setStrokeColorCYMKF self c m y k a
  = liftDOM
      (void
         (self ^. jsf "setStrokeColor"
            [toJSVal c, toJSVal m, toJSVal y, toJSVal k, toJSVal a]))

setFillColorF :: (MonadDOM m, ToJSString color) => CanvasRenderingContext2D -> color -> Maybe Float -> m ()
setFillColorF self color alpha
  = liftDOM
      (void (self ^. jsf "setFillColor" [toJSVal color, toJSVal alpha]))

setFillColorGrayF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Maybe Float -> m ()
setFillColorGrayF self grayLevel alpha
  = liftDOM
      (void
         (self ^. jsf "setFillColor" [toJSVal grayLevel, toJSVal alpha]))

setFillColorRGBF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
setFillColorRGBF self r g b a
  = liftDOM
      (void
         (self ^. jsf "setFillColor"
            [toJSVal r, toJSVal g, toJSVal b, toJSVal a]))

setFillColorCYMKF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> m ()
setFillColorCYMKF self c m y k a
  = liftDOM
      (void
         (self ^. jsf "setFillColor"
            [toJSVal c, toJSVal m, toJSVal y, toJSVal k, toJSVal a]))

strokeRectF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
strokeRectF self x y width height
  = liftDOM
      (void
         (self ^. jsf "strokeRect"
            [toJSVal x, toJSVal y, toJSVal width, toJSVal height]))

drawImageF :: (MonadDOM m, IsCanvasImageSource image) => CanvasRenderingContext2D -> image -> Float -> Float -> m ()
drawImageF self image x y
  = liftDOM
      (void
         (self ^. jsf "drawImage" [toJSVal image, toJSVal x, toJSVal y]))

drawImageScaledF :: (MonadDOM m, IsCanvasImageSource image) => CanvasRenderingContext2D -> image -> Float -> Float -> Float -> Float -> m ()
drawImageScaledF self image x y width height
  = liftDOM
      (void
         (self ^. jsf "drawImage"
            [toJSVal image, toJSVal x, toJSVal y, toJSVal width,
             toJSVal height]))

drawImagePartF :: (MonadDOM m, IsCanvasImageSource image) => CanvasRenderingContext2D -> image -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
drawImagePartF self image sx sy sw sh dx dy dw dh
  = liftDOM
      (void
         (self ^. jsf "drawImage"
            [toJSVal image, toJSVal sx, toJSVal sy, toJSVal sw, toJSVal sh,
             toJSVal dx, toJSVal dy, toJSVal dw, toJSVal dh]))

drawImageFromRectF :: (MonadDOM m, ToJSString compositeOperation) => CanvasRenderingContext2D -> HTMLImageElement -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe Float -> Maybe compositeOperation -> m ()
drawImageFromRectF self image sx sy sw sh dx dy dw dh
  compositeOperation
  = liftDOM
      (void
         (self ^. jsf "drawImageFromRect"
            [toJSVal image, toJSVal sx, toJSVal sy, toJSVal sw, toJSVal sh,
             toJSVal dx, toJSVal dy, toJSVal dw, toJSVal dh,
             toJSVal compositeOperation]))

setShadowF :: (MonadDOM m, ToJSString color) => CanvasRenderingContext2D -> Float -> Float -> Float -> Maybe color -> Maybe Float -> m ()
setShadowF self width height blur color alpha
  = liftDOM
      (void
         (self ^. jsf "setShadow"
            [toJSVal width, toJSVal height, toJSVal blur, toJSVal color,
             toJSVal alpha]))

setShadowGrayF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Maybe Float -> m ()
setShadowGrayF self width height blur grayLevel alpha
  = liftDOM
      (void
         (self ^. jsf "setShadow"
            [toJSVal width, toJSVal height, toJSVal blur, toJSVal grayLevel,
             toJSVal alpha]))

setShadowRGBF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
setShadowRGBF self width height blur r g b a
  = liftDOM
      (void
         (self ^. jsf "setShadow"
            [toJSVal width, toJSVal height, toJSVal blur, toJSVal r, toJSVal g,
             toJSVal b, toJSVal a]))

setShadowCYMKF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
setShadowCYMKF self width height blur c m y k a
  = liftDOM
      (void
         (self ^. jsf "setShadow"
            [toJSVal width, toJSVal height, toJSVal blur, toJSVal c, toJSVal m,
             toJSVal y, toJSVal k, toJSVal a]))

putImageDataF :: (MonadDOM m) => CanvasRenderingContext2D -> ImageData -> Float -> Float -> m ()
putImageDataF self imagedata dx dy
  = liftDOM
      (void
         (self ^. jsf "putImageData"
            [toJSVal imagedata, toJSVal dx, toJSVal dy]))

putImageDataDirtyF :: (MonadDOM m) => CanvasRenderingContext2D -> ImageData -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
putImageDataDirtyF self imagedata dx dy dirtyX dirtyY dirtyWidth
  dirtyHeight
  = liftDOM
      (void
         (self ^. jsf "putImageData"
            [toJSVal imagedata, toJSVal dx, toJSVal dy, toJSVal dirtyX,
             toJSVal dirtyY, toJSVal dirtyWidth, toJSVal dirtyHeight]))

webkitPutImageDataHDF :: (MonadDOM m) => CanvasRenderingContext2D -> ImageData -> Float -> Float -> m ()
webkitPutImageDataHDF self imagedata dx dy
  = liftDOM
      (void
         (self ^. jsf "webkitPutImageDataHD"
            [toJSVal imagedata, toJSVal dx, toJSVal dy]))

webkitPutImageDataHDDirtyF :: (MonadDOM m) => CanvasRenderingContext2D -> ImageData -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
webkitPutImageDataHDDirtyF self imagedata dx dy dirtyX dirtyY
  dirtyWidth dirtyHeight
  = liftDOM
      (void
         (self ^. jsf "webkitPutImageDataHD"
            [toJSVal imagedata, toJSVal dx, toJSVal dy, toJSVal dirtyX,
             toJSVal dirtyY, toJSVal dirtyWidth, toJSVal dirtyHeight]))

createImageDataF :: (MonadDOM m) => CanvasRenderingContext2D -> Maybe ImageData -> m ImageData
createImageDataF self imagedata
  = liftDOM
      ((self ^. jsf "createImageData" [toJSVal imagedata]) >>=
         fromJSValUnchecked)

createImageData_F :: (MonadDOM m) => CanvasRenderingContext2D -> Maybe ImageData -> m ()
createImageData_F self imagedata
  = liftDOM
      (void (self ^. jsf "createImageData" [toJSVal imagedata]))

createImageDataSizeF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> m ImageData
createImageDataSizeF self sw sh
  = liftDOM
      ((self ^. jsf "createImageData" [toJSVal sw, toJSVal sh]) >>=
         fromJSValUnchecked)

createImageDataSize_F :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> m ()
createImageDataSize_F self sw sh
  = liftDOM
      (void (self ^. jsf "createImageData" [toJSVal sw, toJSVal sh]))

createLinearGradientF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m CanvasGradient
createLinearGradientF self x0 y0 x1 y1
  = liftDOM
      ((self ^. jsf "createLinearGradient"
          [toJSVal x0, toJSVal y0, toJSVal x1, toJSVal y1])
         >>= fromJSValUnchecked)

createLinearGradient_F :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
createLinearGradient_F self x0 y0 x1 y1
  = liftDOM
      (void
         (self ^. jsf "createLinearGradient"
            [toJSVal x0, toJSVal y0, toJSVal x1, toJSVal y1]))

createRadialGradientF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> Float -> m CanvasGradient
createRadialGradientF self x0 y0 r0 x1 y1 r1
  = liftDOM
      ((self ^. jsf "createRadialGradient"
          [toJSVal x0, toJSVal y0, toJSVal r0, toJSVal x1, toJSVal y1,
           toJSVal r1])
         >>= fromJSValUnchecked)

createRadialGradient_F :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
createRadialGradient_F self x0 y0 r0 x1 y1 r1
  = liftDOM
      (void
         (self ^. jsf "createRadialGradient"
            [toJSVal x0, toJSVal y0, toJSVal r0, toJSVal x1, toJSVal y1,
             toJSVal r1]))

createPatternF :: (MonadDOM m, IsCanvasImageSource image, ToJSString repetition) => CanvasRenderingContext2D -> image -> repetition -> m (Maybe CanvasPattern)
createPatternF self image repetition
  = liftDOM
      ((self ^. jsf "createPattern" [toJSVal image, toJSVal repetition])
         >>= fromJSVal)

createPattern_F :: (MonadDOM m, IsCanvasImageSource image, ToJSString repetition) => CanvasRenderingContext2D -> image -> repetition -> m ()
createPattern_F self image repetition
  = liftDOM
      (void
         (self ^. jsf "createPattern" [toJSVal image, toJSVal repetition]))

createPatternUnsafeF :: (MonadDOM m, IsCanvasImageSource image, ToJSString repetition,
                     HasCallStack) => CanvasRenderingContext2D -> image -> repetition -> m CanvasPattern
createPatternUnsafeF self image repetition
  = liftDOM
      (((self ^. jsf "createPattern" [toJSVal image, toJSVal repetition])
          >>= fromJSVal)
         >>= maybe (Prelude.error "Nothing to return") return)

createPatternUncheckedF :: (MonadDOM m, IsCanvasImageSource image, ToJSString repetition) => CanvasRenderingContext2D -> image -> repetition -> m CanvasPattern
createPatternUncheckedF self image repetition
  = liftDOM
      ((self ^. jsf "createPattern" [toJSVal image, toJSVal repetition])
         >>= fromJSValUnchecked)

getImageDataF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ImageData
getImageDataF self sx sy sw sh
  = liftDOM
      ((self ^. jsf "getImageData"
          [toJSVal sx, toJSVal sy, toJSVal sw, toJSVal sh])
         >>= fromJSValUnchecked)

getImageData_F :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
getImageData_F self sx sy sw sh
  = liftDOM
      (void
         (self ^. jsf "getImageData"
            [toJSVal sx, toJSVal sy, toJSVal sw, toJSVal sh]))

webkitGetImageDataHDF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ImageData
webkitGetImageDataHDF self sx sy sw sh
  = liftDOM
      ((self ^. jsf "webkitGetImageDataHD"
          [toJSVal sx, toJSVal sy, toJSVal sw, toJSVal sh])
         >>= fromJSValUnchecked)

webkitGetImageDataHD_F :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> Float -> Float -> Float -> m ()
webkitGetImageDataHD_F self sx sy sw sh
  = liftDOM
      (void
         (self ^. jsf "webkitGetImageDataHD"
            [toJSVal sx, toJSVal sy, toJSVal sw, toJSVal sh]))

drawFocusIfNeededF :: (MonadDOM m, IsElement element) => CanvasRenderingContext2D -> element -> m ()
drawFocusIfNeededF self element
  = liftDOM
      (void (self ^. jsf "drawFocusIfNeeded" [toJSVal element]))

drawFocusIfNeededPathF :: (MonadDOM m, IsElement element) => CanvasRenderingContext2D -> Path2D -> element -> m ()
drawFocusIfNeededPathF self path element
  = liftDOM
      (void
         (self ^. jsf "drawFocusIfNeeded" [toJSVal path, toJSVal element]))

getCanvasF :: (MonadDOM m) => CanvasRenderingContext2D -> m HTMLCanvasElement
getCanvasF self
  = liftDOM ((self ^. js "canvas") >>= fromJSValUnchecked)

setGlobalAlphaF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setGlobalAlphaF self val
  = liftDOM (self ^. jss "globalAlpha" (toJSVal val))

getGlobalAlphaF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getGlobalAlphaF self
  = liftDOM
      (realToFrac <$> ((self ^. js "globalAlpha") >>= valToNumber))

setGlobalCompositeOperationF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setGlobalCompositeOperationF self val
  = liftDOM (self ^. jss "globalCompositeOperation" (toJSVal val))

getGlobalCompositeOperationF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getGlobalCompositeOperationF self
  = liftDOM
      ((self ^. js "globalCompositeOperation") >>= fromJSValUnchecked)

setLineWidthF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setLineWidthF self val
  = liftDOM (self ^. jss "lineWidth" (toJSVal val))

getLineWidthF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getLineWidthF self
  = liftDOM
      (realToFrac <$> ((self ^. js "lineWidth") >>= valToNumber))

setLineCapF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setLineCapF self val = liftDOM (self ^. jss "lineCap" (toJSVal val))

getLineCapF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getLineCapF self
  = liftDOM ((self ^. js "lineCap") >>= fromJSValUnchecked)

setLineJoinF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setLineJoinF self val
  = liftDOM (self ^. jss "lineJoin" (toJSVal val))

getLineJoinF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getLineJoinF self
  = liftDOM ((self ^. js "lineJoin") >>= fromJSValUnchecked)

setMiterLimitF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setMiterLimitF self val
  = liftDOM (self ^. jss "miterLimit" (toJSVal val))

getMiterLimitF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getMiterLimitF self
  = liftDOM
      (realToFrac <$> ((self ^. js "miterLimit") >>= valToNumber))

setShadowOffsetXF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setShadowOffsetXF self val
  = liftDOM (self ^. jss "shadowOffsetX" (toJSVal val))

getShadowOffsetXF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getShadowOffsetXF self
  = liftDOM
      (realToFrac <$> ((self ^. js "shadowOffsetX") >>= valToNumber))

setShadowOffsetYF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setShadowOffsetYF self val
  = liftDOM (self ^. jss "shadowOffsetY" (toJSVal val))

getShadowOffsetYF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getShadowOffsetYF self
  = liftDOM
      (realToFrac <$> ((self ^. js "shadowOffsetY") >>= valToNumber))

setShadowBlurF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setShadowBlurF self val
  = liftDOM (self ^. jss "shadowBlur" (toJSVal val))

getShadowBlurF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getShadowBlurF self
  = liftDOM
      (realToFrac <$> ((self ^. js "shadowBlur") >>= valToNumber))

setShadowColorF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setShadowColorF self val
  = liftDOM (self ^. jss "shadowColor" (toJSVal val))

getShadowColorF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getShadowColorF self
  = liftDOM ((self ^. js "shadowColor") >>= fromJSValUnchecked)

setLineDashOffsetF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setLineDashOffsetF self val
  = liftDOM (self ^. jss "lineDashOffset" (toJSVal val))

getLineDashOffsetF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getLineDashOffsetF self
  = liftDOM
      (realToFrac <$> ((self ^. js "lineDashOffset") >>= valToNumber))

setWebkitLineDashF :: (MonadDOM m) => CanvasRenderingContext2D -> [Float] -> m ()
setWebkitLineDashF self val
  = liftDOM (self ^. jss "webkitLineDash" (toJSVal (array val)))

getWebkitLineDashF :: (MonadDOM m) => CanvasRenderingContext2D -> m [Float]
getWebkitLineDashF self
  = liftDOM ((self ^. js "webkitLineDash") >>= fromJSArrayUnchecked)

setWebkitLineDashOffsetF :: (MonadDOM m) => CanvasRenderingContext2D -> Float -> m ()
setWebkitLineDashOffsetF self val
  = liftDOM (self ^. jss "webkitLineDashOffset" (toJSVal val))

getWebkitLineDashOffsetF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getWebkitLineDashOffsetF self
  = liftDOM
      (realToFrac <$>
         ((self ^. js "webkitLineDashOffset") >>= valToNumber))

setFontF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setFontF self val = liftDOM (self ^. jss "font" (toJSVal val))

getFontF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getFontF self = liftDOM ((self ^. js "font") >>= fromJSValUnchecked)

setTextAlignF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setTextAlignF self val
  = liftDOM (self ^. jss "textAlign" (toJSVal val))

getTextAlignF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getTextAlignF self
  = liftDOM ((self ^. js "textAlign") >>= fromJSValUnchecked)

setTextBaselineF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setTextBaselineF self val
  = liftDOM (self ^. jss "textBaseline" (toJSVal val))

getTextBaselineF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getTextBaselineF self
  = liftDOM ((self ^. js "textBaseline") >>= fromJSValUnchecked)

setDirectionF :: (MonadDOM m, ToJSString val) => CanvasRenderingContext2D -> val -> m ()
setDirectionF self val
  = liftDOM (self ^. jss "direction" (toJSVal val))

getDirectionF :: (MonadDOM m, FromJSString result) => CanvasRenderingContext2D -> m result
getDirectionF self
  = liftDOM ((self ^. js "direction") >>= fromJSValUnchecked)

setStrokeStyleF :: (MonadDOM m, IsCanvasStyle val) => CanvasRenderingContext2D -> val -> m ()
setStrokeStyleF self val
  = liftDOM (self ^. jss "strokeStyle" (toJSVal val))

getStrokeStyleF :: (MonadDOM m) => CanvasRenderingContext2D -> m CanvasStyle
getStrokeStyleF self
  = liftDOM ((self ^. js "strokeStyle") >>= fromJSValUnchecked)

setFillStyleF :: (MonadDOM m, IsCanvasStyle val) => CanvasRenderingContext2D -> val -> m ()
setFillStyleF self val
  = liftDOM (self ^. jss "fillStyle" (toJSVal val))

getFillStyleF :: (MonadDOM m) => CanvasRenderingContext2D -> m CanvasStyle
getFillStyleF self
  = liftDOM ((self ^. js "fillStyle") >>= fromJSValUnchecked)

getWebkitBackingStorePixelRatioF :: (MonadDOM m) => CanvasRenderingContext2D -> m Float
getWebkitBackingStorePixelRatioF self
  = liftDOM
      (realToFrac <$>
         ((self ^. js "webkitBackingStorePixelRatio") >>= valToNumber))

setImageSmoothingEnabledF :: (MonadDOM m) => CanvasRenderingContext2D -> Bool -> m ()
setImageSmoothingEnabledF self val
  = liftDOM (self ^. jss "imageSmoothingEnabled" (toJSVal val))

getImageSmoothingEnabledF :: (MonadDOM m) => CanvasRenderingContext2D -> m Bool
getImageSmoothingEnabledF self
  = liftDOM ((self ^. js "imageSmoothingEnabled") >>= valToBool)

setWebkitImageSmoothingEnabledF :: (MonadDOM m) => CanvasRenderingContext2D -> Bool -> m ()
setWebkitImageSmoothingEnabledF self val
  = liftDOM (self ^. jss "webkitImageSmoothingEnabled" (toJSVal val))

getWebkitImageSmoothingEnabledF :: (MonadDOM m) => CanvasRenderingContext2D -> m Bool
getWebkitImageSmoothingEnabledF self
  = liftDOM
      ((self ^. js "webkitImageSmoothingEnabled") >>= valToBool)

setImageSmoothingQualityF :: (MonadDOM m) => CanvasRenderingContext2D -> ImageSmoothingQuality -> m ()
setImageSmoothingQualityF self val
  = liftDOM (self ^. jss "imageSmoothingQuality" (toJSVal val))

getImageSmoothingQualityF :: (MonadDOM m) => CanvasRenderingContext2D -> m ImageSmoothingQuality
getImageSmoothingQualityF self
  = liftDOM
      ((self ^. js "imageSmoothingQuality") >>= fromJSValUnchecked)
