{
  description = "Merkle Patricia Forestry implementation in Haskell";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs = { follows = "haskellNix/nixpkgs-unstable"; };
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };

  outputs =
    inputs@{ self, nixpkgs, flake-utils, haskellNix, ... }:
    let
      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      perSystem = system:
        let
          pkgs = import nixpkgs {
            overlays = [ haskellNix.overlay ];
            inherit system;
          };

          project = pkgs.haskell-nix.cabalProject' {
            name = "haskell-mpf";
            src = ./.;
            compiler-nix-name = "ghc984";
            shell = {
              tools = {
                cabal = { index-state = "2025-10-01T00:00:00Z"; };
                cabal-fmt = { index-state = "2025-10-01T00:00:00Z"; };
                haskell-language-server = {
                  index-state = "2025-10-01T00:00:00Z";
                };
                hoogle = { index-state = "2025-10-01T00:00:00Z"; };
                fourmolu = { index-state = "2025-10-01T00:00:00Z"; };
                hlint = { index-state = "2025-10-01T00:00:00Z"; };
              };
              withHoogle = true;
              buildInputs = [
                pkgs.just
                pkgs.nixfmt-classic
                pkgs.shellcheck
              ];
              shellHook = ''
                echo "Entering haskell-mpf dev shell"
              '';
            };
          };

        in {
          packages = {
            default =
              project.hsPkgs.haskell-mpf.components.library;
            unit-tests =
              project.hsPkgs.haskell-mpf.components.tests.unit-tests;
          };
          devShells.default = project.shell;
        };

    in flake-utils.lib.eachSystem
      [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
