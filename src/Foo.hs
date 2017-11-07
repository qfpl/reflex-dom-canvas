{-# LANGUAGE DeriveFunctor #-}
module Foo where

import           JSDOM.CanvasRenderingContext2D as C

import           JSDOM.Generated.Enums          (CanvasWindingRule,
                                                 ImageSmoothingQuality)

import           JSDOM.Types                    (CanvasGradient,
                                                 CanvasImageSource,
                                                 CanvasPattern, CanvasStyle,
                                                 Element, HTMLCanvasElement,
                                                 HTMLImageElement, ImageData,
                                                 JSString, Path2D, TextMetrics)

data SaddleCanvasF a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.save Mozilla CanvasRenderingContext2D.save documentation>
  = Save CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.restore Mozilla CanvasRenderingContext2D.restore documentation>
  | Restore CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.commit Mozilla CanvasRenderingContext2D.commit documentation>
  | Commit CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.scale Mozilla CanvasRenderingContext2D.scale documentation>
  | Scale CanvasRenderingContext2D Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.rotate Mozilla CanvasRenderingContext2D.rotate documentation>
  | Rotate CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.translate Mozilla CanvasRenderingContext2D.translate documentation>
  | Translate CanvasRenderingContext2D Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.transform Mozilla CanvasRenderingContext2D.transform documentation>
  | Transform CanvasRenderingContext2D Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setTransform Mozilla CanvasRenderingContext2D.setTransform documentation>
  | SetTransform CanvasRenderingContext2D Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.resetTransform Mozilla CanvasRenderingContext2D.resetTransform documentation>
  | ResetTransform CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setLineDash Mozilla CanvasRenderingContext2D.setLineDash documentation>
  | SetLineDash CanvasRenderingContext2D [Float] a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.getLineDash Mozilla CanvasRenderingContext2D.getLineDash documentation>
  | GetLineDash CanvasRenderingContext2D ([Float] -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.getLineDash Mozilla CanvasRenderingContext2D.getLineDash documentation>
  | GetLineDash_ CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.clearRect Mozilla CanvasRenderingContext2D.clearRect documentation>
  | ClearRect CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.fillRect Mozilla CanvasRenderingContext2D.fillRect documentation>
  | FillRect CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.beginPath Mozilla CanvasRenderingContext2D.beginPath documentation>
  | BeginPath CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.fill Mozilla CanvasRenderingContext2D.fill documentation>
  | FillPath CanvasRenderingContext2D Path2D (Maybe CanvasWindingRule) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.stroke Mozilla CanvasRenderingContext2D.stroke documentation>
  | StrokePath CanvasRenderingContext2D Path2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.clip Mozilla CanvasRenderingContext2D.clip documentation>
  | ClipPath CanvasRenderingContext2D Path2D (Maybe CanvasWindingRule) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.fill Mozilla CanvasRenderingContext2D.fill documentation>
  | Fill CanvasRenderingContext2D (Maybe CanvasWindingRule) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.stroke Mozilla CanvasRenderingContext2D.stroke documentation>
  | Stroke CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.clip Mozilla CanvasRenderingContext2D.clip documentation>
  | Clip CanvasRenderingContext2D (Maybe CanvasWindingRule) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInPath Mozilla CanvasRenderingContext2D.isPointInPath documentation>
  | IsPointInPathPath CanvasRenderingContext2D Path2D Float Float (Maybe CanvasWindingRule) (Bool -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInPath Mozilla CanvasRenderingContext2D.isPointInPath documentation>
  | IsPointInPathPath_ CanvasRenderingContext2D Path2D Float Float (Maybe CanvasWindingRule) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInStroke Mozilla CanvasRenderingContext2D.isPointInStroke documentation>
  | IsPointInStrokePath CanvasRenderingContext2D Path2D Float Float (Bool -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInStroke Mozilla CanvasRenderingContext2D.isPointInStroke documentation>
  | IsPointInStrokePath_ CanvasRenderingContext2D Path2D Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInPath Mozilla CanvasRenderingContext2D.isPointInPath documentation>
  | IsPointInPath CanvasRenderingContext2D Float Float (Maybe CanvasWindingRule) (Bool -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInPath Mozilla CanvasRenderingContext2D.isPointInPath documentation>
  | IsPointInPath_ CanvasRenderingContext2D Float Float (Maybe CanvasWindingRule) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInStroke Mozilla CanvasRenderingContext2D.isPointInStroke documentation>
  | IsPointInStroke CanvasRenderingContext2D Float Float (Bool -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.isPointInStroke Mozilla CanvasRenderingContext2D.isPointInStroke documentation>
  | IsPointInStroke_ CanvasRenderingContext2D Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.measureText Mozilla CanvasRenderingContext2D.measureText documentation>
  | MeasureText CanvasRenderingContext2D JSString (TextMetrics -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.measureText Mozilla CanvasRenderingContext2D.measureText documentation>
  | MeasureText_ CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setAlpha Mozilla CanvasRenderingContext2D.setAlpha documentation>
  | SetAlpha CanvasRenderingContext2D (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setCompositeOperation Mozilla CanvasRenderingContext2D.setCompositeOperation documentation>
  | SetCompositeOperation CanvasRenderingContext2D (Maybe JSString) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setLineWidth Mozilla CanvasRenderingContext2D.setLineWidth documentation>
  | SetLineWidthFunction CanvasRenderingContext2D (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setLineCap Mozilla CanvasRenderingContext2D.setLineCap documentation>
  | SetLineCapFunction CanvasRenderingContext2D (Maybe JSString) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setLineJoin Mozilla CanvasRenderingContext2D.setLineJoin documentation>
  | SetLineJoinFunction CanvasRenderingContext2D (Maybe JSString) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setMiterLimit Mozilla CanvasRenderingContext2D.setMiterLimit documentation>
  | SetMiterLimitFunction CanvasRenderingContext2D (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.clearShadow Mozilla CanvasRenderingContext2D.clearShadow documentation>
  | ClearShadow CanvasRenderingContext2D a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.fillText Mozilla CanvasRenderingContext2D.fillText documentation>
  | FillText CanvasRenderingContext2D JSString Float Float (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.strokeText Mozilla CanvasRenderingContext2D.strokeText documentation>
  | StrokeText CanvasRenderingContext2D JSString Float Float (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setStrokeColor Mozilla CanvasRenderingContext2D.setStrokeColor documentation>
  | SetStrokeColor CanvasRenderingContext2D JSString (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setStrokeColor Mozilla CanvasRenderingContext2D.setStrokeColor documentation>
  | SetStrokeColorGray CanvasRenderingContext2D Float (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setStrokeColor Mozilla CanvasRenderingContext2D.setStrokeColor documentation>
  | SetStrokeColorRGB CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setStrokeColor Mozilla CanvasRenderingContext2D.setStrokeColor documentation>
  | SetStrokeColorCYMK CanvasRenderingContext2D Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setFillColor Mozilla CanvasRenderingContext2D.setFillColor documentation>
  | SetFillColor CanvasRenderingContext2D JSString (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setFillColor Mozilla CanvasRenderingContext2D.setFillColor documentation>
  | SetFillColorGray CanvasRenderingContext2D Float (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setFillColor Mozilla CanvasRenderingContext2D.setFillColor documentation>
  | SetFillColorRGB CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setFillColor Mozilla CanvasRenderingContext2D.setFillColor documentation>
  | SetFillColorCYMK CanvasRenderingContext2D Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.strokeRect Mozilla CanvasRenderingContext2D.strokeRect documentation>
  | StrokeRect CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.drawImage Mozilla CanvasRenderingContext2D.drawImage documentation>
  | DrawImage CanvasRenderingContext2D CanvasImageSource Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.drawImage Mozilla CanvasRenderingContext2D.drawImage documentation>
  | DrawImageScaled CanvasRenderingContext2D CanvasImageSource Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.drawImage Mozilla CanvasRenderingContext2D.drawImage documentation>
  | DrawImagePart CanvasRenderingContext2D CanvasImageSource Float Float Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.drawImageFromRect Mozilla CanvasRenderingContext2D.drawImageFromRect documentation>
  | DrawImageFromRect CanvasRenderingContext2D HTMLImageElement (Maybe Float) (Maybe Float) (Maybe Float) (Maybe Float) (Maybe Float) (Maybe Float) (Maybe Float) (Maybe Float) (Maybe JSString) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setShadow Mozilla CanvasRenderingContext2D.setShadow documentation>
  | SetShadow CanvasRenderingContext2D Float Float Float (Maybe JSString) (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setShadow Mozilla CanvasRenderingContext2D.setShadow documentation>
  | SetShadowGray CanvasRenderingContext2D Float Float Float Float (Maybe Float) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setShadow Mozilla CanvasRenderingContext2D.setShadow documentation>
  | SetShadowRGB CanvasRenderingContext2D Float Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.setShadow Mozilla CanvasRenderingContext2D.setShadow documentation>
  | SetShadowCYMK CanvasRenderingContext2D Float Float Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.putImageData Mozilla CanvasRenderingContext2D.putImageData documentation>
  | PutImageData CanvasRenderingContext2D ImageData Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.putImageData Mozilla CanvasRenderingContext2D.putImageData documentation>
  | PutImageDataDirty CanvasRenderingContext2D ImageData Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitPutImageDataHD Mozilla CanvasRenderingContext2D.webkitPutImageDataHD documentation>
  | WebkitPutImageDataHD CanvasRenderingContext2D ImageData Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitPutImageDataHD Mozilla CanvasRenderingContext2D.webkitPutImageDataHD documentation>
  | WebkitPutImageDataHDDirty CanvasRenderingContext2D ImageData Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createImageData Mozilla CanvasRenderingContext2D.createImageData documentation>
  | CreateImageData CanvasRenderingContext2D (Maybe ImageData) (ImageData -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createImageData Mozilla CanvasRenderingContext2D.createImageData documentation>
  | CreateImageData_ CanvasRenderingContext2D (Maybe ImageData) a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createImageData Mozilla CanvasRenderingContext2D.createImageData documentation>
  | CreateImageDataSize CanvasRenderingContext2D Float Float (ImageData -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createImageData Mozilla CanvasRenderingContext2D.createImageData documentation>
  | CreateImageDataSize_ CanvasRenderingContext2D Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createLinearGradient Mozilla CanvasRenderingContext2D.createLinearGradient documentation>
  | CreateLinearGradient CanvasRenderingContext2D Float Float Float Float (CanvasGradient -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createLinearGradient Mozilla CanvasRenderingContext2D.createLinearGradient documentation>
  | CreateLinearGradient_ CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createRadialGradient Mozilla CanvasRenderingContext2D.createRadialGradient documentation>
  | CreateRadialGradient CanvasRenderingContext2D Float Float Float Float Float Float (CanvasGradient -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createRadialGradient Mozilla CanvasRenderingContext2D.createRadialGradient documentation>
  | CreateRadialGradient_ CanvasRenderingContext2D Float Float Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createPattern Mozilla CanvasRenderingContext2D.createPattern documentation>
  | CreatePattern CanvasRenderingContext2D CanvasImageSource JSString (Maybe CanvasPattern -> a )
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createPattern Mozilla CanvasRenderingContext2D.createPattern documentation>
  | CreatePattern_ CanvasRenderingContext2D CanvasImageSource JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createPattern Mozilla CanvasRenderingContext2D.createPattern documentation>
  | CreatePatternUnsafe CanvasRenderingContext2D CanvasImageSource JSString ( CanvasPattern -> a )
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.createPattern Mozilla CanvasRenderingContext2D.createPattern documentation>
  | CreatePatternUnchecked CanvasRenderingContext2D CanvasImageSource JSString (CanvasPattern -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.getImageData Mozilla CanvasRenderingContext2D.getImageData documentation>
  | GetImageData CanvasRenderingContext2D Float Float Float Float (ImageData -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.getImageData Mozilla CanvasRenderingContext2D.getImageData documentation>
  | GetImageData_ CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitGetImageDataHD Mozilla CanvasRenderingContext2D.webkitGetImageDataHD documentation>
  | WebkitGetImageDataHD CanvasRenderingContext2D Float Float Float Float (ImageData -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitGetImageDataHD Mozilla CanvasRenderingContext2D.webkitGetImageDataHD documentation>
  | WebkitGetImageDataHD_ CanvasRenderingContext2D Float Float Float Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.drawFocusIfNeeded Mozilla CanvasRenderingContext2D.drawFocusIfNeeded documentation>
  | DrawFocusIfNeeded CanvasRenderingContext2D Element a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.drawFocusIfNeeded Mozilla CanvasRenderingContext2D.drawFocusIfNeeded documentation>
  | DrawFocusIfNeededPath CanvasRenderingContext2D Path2D Element a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.canvas Mozilla CanvasRenderingContext2D.canvas documentation>
  | GetCanvas CanvasRenderingContext2D (HTMLCanvasElement -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.globalAlpha Mozilla CanvasRenderingContext2D.globalAlpha documentation>
  | SetGlobalAlpha CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.globalAlpha Mozilla CanvasRenderingContext2D.globalAlpha documentation>
  | GetGlobalAlpha CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.globalCompositeOperation Mozilla CanvasRenderingContext2D.globalCompositeOperation documentation>
  | SetGlobalCompositeOperation CanvasRenderingContext2D String a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.globalCompositeOperation Mozilla CanvasRenderingContext2D.globalCompositeOperation documentation>
  | GetGlobalCompositeOperation CanvasRenderingContext2D (JSString -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineWidth Mozilla CanvasRenderingContext2D.lineWidth documentation>
  | SetLineWidth CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineWidth Mozilla CanvasRenderingContext2D.lineWidth documentation>
  | GetLineWidth CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineCap Mozilla CanvasRenderingContext2D.lineCap documentation>
  | SetLineCap CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineCap Mozilla CanvasRenderingContext2D.lineCap documentation>
  | GetLineCap CanvasRenderingContext2D (JSString -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineJoin Mozilla CanvasRenderingContext2D.lineJoin documentation>
  | SetLineJoin CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineJoin Mozilla CanvasRenderingContext2D.lineJoin documentation>
  | GetLineJoin CanvasRenderingContext2D (JSString -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.miterLimit Mozilla CanvasRenderingContext2D.miterLimit documentation>
  | SetMiterLimit CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.miterLimit Mozilla CanvasRenderingContext2D.miterLimit documentation>
  | GetMiterLimit CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowOffsetX Mozilla CanvasRenderingContext2D.shadowOffsetX documentation>
  | SetShadowOffsetX CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowOffsetX Mozilla CanvasRenderingContext2D.shadowOffsetX documentation>
  | GetShadowOffsetX CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowOffsetY Mozilla CanvasRenderingContext2D.shadowOffsetY documentation>
  | SetShadowOffsetY CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowOffsetY Mozilla CanvasRenderingContext2D.shadowOffsetY documentation>
  | GetShadowOffsetY CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowBlur Mozilla CanvasRenderingContext2D.shadowBlur documentation>
  | SetShadowBlur CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowBlur Mozilla CanvasRenderingContext2D.shadowBlur documentation>
  | GetShadowBlur CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowColor Mozilla CanvasRenderingContext2D.shadowColor documentation>
  | SetShadowColor CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.shadowColor Mozilla CanvasRenderingContext2D.shadowColor documentation>
  | GetShadowColor CanvasRenderingContext2D (JSString -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineDashOffset Mozilla CanvasRenderingContext2D.lineDashOffset documentation>
  | SetLineDashOffset CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.lineDashOffset Mozilla CanvasRenderingContext2D.lineDashOffset documentation>
  | GetLineDashOffset CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitLineDash Mozilla CanvasRenderingContext2D.webkitLineDash documentation>
  | SetWebkitLineDash CanvasRenderingContext2D [Float] a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitLineDash Mozilla CanvasRenderingContext2D.webkitLineDash documentation>
  | GetWebkitLineDash CanvasRenderingContext2D ([Float] -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitLineDashOffset Mozilla CanvasRenderingContext2D.webkitLineDashOffset documentation>
  | SetWebkitLineDashOffset CanvasRenderingContext2D Float a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitLineDashOffset Mozilla CanvasRenderingContext2D.webkitLineDashOffset documentation>
  | GetWebkitLineDashOffset CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.font Mozilla CanvasRenderingContext2D.font documentation>
  | SetFont CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.font Mozilla CanvasRenderingContext2D.font documentation>
  | GetFont CanvasRenderingContext2D (JSString -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.textAlign Mozilla CanvasRenderingContext2D.textAlign documentation>
  | SetTextAlign CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.textAlign Mozilla CanvasRenderingContext2D.textAlign documentation>
  | GetTextAlign CanvasRenderingContext2D (JSString -> a) -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.textBaseline Mozilla CanvasRenderingContext2D.textBaseline documentation>
  | SetTextBaseline CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.textBaseline Mozilla CanvasRenderingContext2D.textBaseline documentation>
  | GetTextBaseline CanvasRenderingContext2D (JSString -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.direction Mozilla CanvasRenderingContext2D.direction documentation>
  | SetDirection CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.direction Mozilla CanvasRenderingContext2D.direction documentation>
  | GetDirection CanvasRenderingContext2D (JSString -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.strokeStyle Mozilla CanvasRenderingContext2D.strokeStyle documentation>
  | SetStrokeStyle CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.strokeStyle Mozilla CanvasRenderingContext2D.strokeStyle documentation>
  | GetStrokeStyle CanvasRenderingContext2D (CanvasStyle -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.fillStyle Mozilla CanvasRenderingContext2D.fillStyle documentation>
  | SetFillStyle CanvasRenderingContext2D JSString a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.fillStyle Mozilla CanvasRenderingContext2D.fillStyle documentation>
  | GetFillStyle CanvasRenderingContext2D (CanvasStyle -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitBackingStorePixelRatio Mozilla CanvasRenderingContext2D.webkitBackingStorePixelRatio documentation>
  | GetWebkitBackingStorePixelRatio CanvasRenderingContext2D (Float -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.imageSmoothingEnabled Mozilla CanvasRenderingContext2D.imageSmoothingEnabled documentation>
  | SetImageSmoothingEnabled CanvasRenderingContext2D Bool a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.imageSmoothingEnabled Mozilla CanvasRenderingContext2D.imageSmoothingEnabled documentation>
  | GetImageSmoothingEnabled CanvasRenderingContext2D (Bool -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitImageSmoothingEnabled Mozilla CanvasRenderingContext2D.webkitImageSmoothingEnabled documentation>
  | SetWebkitImageSmoothingEnabled CanvasRenderingContext2D Bool a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.webkitImageSmoothingEnabled Mozilla CanvasRenderingContext2D.webkitImageSmoothingEnabled documentation>
  | GetWebkitImageSmoothingEnabled CanvasRenderingContext2D (Bool -> a)
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.imageSmoothingQuality Mozilla CanvasRenderingContext2D.imageSmoothingQuality documentation>
  | SetImageSmoothingQuality CanvasRenderingContext2D ImageSmoothingQuality a
  -- | <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D.imageSmoothingQuality Mozilla CanvasRenderingContext2D.imageSmoothingQuality documentation>
  | GetImageSmoothingQuality CanvasRenderingContext2D (ImageSmoothingQuality -> a)
  deriving (Functor)
