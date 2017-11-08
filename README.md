# Reflex HTML5 Canvas

Initial work on integrating the HTML5 Canvas with Reflex-Dom.

Currently uses another transformer layer to manage the context for the canvas
element you would like to draw on, so multiple canvas elements will need to be
managed by the user, independently.


## TODO / NB

There is a lot of typeclass instance duplication that I would like to remove, if
possible. Most are already managed by the ``MonadWidget t m`` constraint so the
instances I use simply handball things further up the chain.

There is a ``PrimMonad`` constraint that bubbles up to the user, this needs to
be handled and discharged appropriately as ``MonadWidget`` should be sufficient
to handle everything I need.

The type system should prevent you from trying to use a
``WebGLRenderingContext`` when you need to use a ``Canvas2DRenderingContext``
and vice versa. That should prevent silly JS errors from calling the wrong
function on the wrong context object.

There are no functions implemented for the ``WebGLRenderingContext`` yet, and
the render function isn't implemented. Also only a bare minimum of the
``Canvas2DRenderingContext`` functions have been implemented. Sufficient for
basic testing, but nothing more.

I'm not convinced I need a monad transformer for this. What I think I need to be
doing is returning a ``Dynamic t X`` where ``X`` is something that is to be
filled with ``CanvasM`` type things. That I can draw later... But I don't have a
full picture in my head of how this will work.
