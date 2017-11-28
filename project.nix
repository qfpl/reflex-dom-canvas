(import ../reflex-platform {}).project ({ pkgs }: {
  packages = {
    frontend = ./.;
  };

  shells = {
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };
})
