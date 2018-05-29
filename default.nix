{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  pkgs = reflex-platform.nixpkgs.pkgs;

  adjust-for-ghcjs = drv: {
    doHaddock = false;
  };

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    });
  };

  adjust = drv:
    if compiler == "ghcjs"
    then adjust-for-ghcjs drv
    else drv;

  basics = pkgs.haskell.lib.overrideCabal (
    haskellPackages.callPackage ./reflex-dom-canvas.nix {}
  ) adjust;

in
  basics
