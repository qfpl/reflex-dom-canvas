# Reflex HTML5 Canvas

Helper functions for creating and managing a Canvas element and a [RenderingContext](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext). Supports both Context2d and Webgl. 

The main functions for creating the canvas and retrieving the ``Dynamic`` containing the requested context are in ``Reflex.Dom.CanvasDyn``. Use either ``dContext2d`` or ``dContextWebgl`` to build your canvas. It will return a ``Dynamic`` containing a record with:

```haskell
data CanvasInfo (c :: ContextType) t = CanvasInfo
  { _canvasInfo_El       :: El t              -- Canvas HTML element
  , _canvasInfo_context  :: RenderContext c   -- RenderingContext JS object for the context (2d/webgl) that you can requested.
  , _canvasInfo_keyEvent :: Key -> Event t () -- Function, takes a Key, returns an Event for when that Key is pressed on the Canvas element
```

The functions ``drawWithCx`` and ``drawCanvasFree`` are for applying instructions to the canvas using ``Dynamic t ( RenderContext c )``.

There are two modules ``Reflex.Dom.Canvas.WebGL`` and ``Reflex.Dom.Canvas.Context2D`` that contain a ``Free`` monad implementation of some canvas functions for their respective contexts. These are not complete implementations, but may be useful to someone wanting to experiment with a more FP friendly way of interacting with the Canvas API.
