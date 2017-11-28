{ mkDerivation, base, bifunctors, containers, free, jsaddle
, jsaddle-dom, jsaddle-warp, lens, mtl, random, reflex, reflex-dom
, reflex-dom-core, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors containers free jsaddle jsaddle-dom jsaddle-warp
    lens mtl random reflex reflex-dom reflex-dom-core text time
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
