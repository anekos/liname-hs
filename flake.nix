{
  description = "liname-hs - Image renaming tool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            liname-hs = final.callCabal2nix "liname-hs" ./. { };
          };
        };

        liname-hs = pkgs.haskell.lib.compose.overrideCabal (drv: {
          jailbreak = true;
          doCheck = false;
        }) haskellPackages.liname-hs;

      in
      {
        packages = {
          default = liname-hs;
          liname-hs = liname-hs;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            ormolu
          ];

          inputsFrom = [ liname-hs ];
        };

        apps.default = {
          type = "app";
          program = "${liname-hs}/bin/liname";
        };
        apps.undo-liname = {
          type = "app";
          program = "${liname-hs}/bin/undo-liname";
        };
      }
    );
}
