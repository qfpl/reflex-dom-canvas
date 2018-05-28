{ mkDerivation, base, bifunctors, containers, free, jsaddle
, jsaddle-dom, lens, mtl, random, reflex, reflex-dom-core, stdenv
, text, time
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bifunctors containers free jsaddle jsaddle-dom lens mtl random
    reflex reflex-dom-core text time
  ];
  description = "Reflex functions for the HTML5 Canvas (2D & WebGL)";
  license = stdenv.lib.licenses.bsd3;
}
