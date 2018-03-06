{ reflex-platform ? import ./reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    });
  };

  adjust-for-ghcjs = drv: {
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out

      mkdir -p $out/js
      cp $out/bin/reflex-dom-canvas.jsexe/all.js $out/js/reflex-dom-canvas.js

      mkdir -p $out/css
      ln -s ./css/* $out/css

      ln -s $out/bin/reflex-dom-canvas.jsexe/index.html $out/index.html

      cd $out/bin/reflex-dom-canvas.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/reflex-dom-canvas.min.js
      rm -Rf $out/bin/reflex-dom-canvas.jsexe
      rm -Rf $out/bin

      cd $out/js
      zopfli -i1000 reflex-dom-canvas.min.js

      rm -Rf $out/lib
      rm -Rf $out/nix-support
      rm -Rf $out/share
    '';
  };

  adjust = drv:
    if compiler == "ghcjs"
    then adjust-for-ghcjs drv
    else drv;

  basics = pkgs.haskell.lib.overrideCabal (haskellPackages.callPackage ./reflex-dom-canvas.nix {}) adjust;
in
  basics
