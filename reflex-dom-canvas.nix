{ mkDerivation, base, bifunctors, containers, directory, filepath
, free, jsaddle, jsaddle-dom, jsaddle-warp, lens, mtl, reflex
, reflex-dom, reflex-dom-core, stdenv, text, wai, exception-transformers, primitive, ref-tf, monad-control
, wai-middleware-static, warp, websockets, random
}:
mkDerivation {
  pname = "reflex-dom-canvas";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors containers directory filepath free
    jsaddle jsaddle-warp jsaddle-dom lens mtl reflex reflex-dom reflex-dom-core
    text wai wai-middleware-static warp websockets random
    exception-transformers primitive ref-tf monad-control
  ];
  executableHaskellDepends = [ base reflex-dom-core ];
  license = stdenv.lib.licenses.bsd3;
}
