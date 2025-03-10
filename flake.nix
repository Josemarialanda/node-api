{
  description = "User authentication API";

  inputs = {
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "node-api";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: rec {
            openapi3 =
              pkgs.lib.pipe super.openapi3
                [
                  pkgs.haskell.lib.unmarkBroken
                  pkgs.haskell.lib.dontCheck
                ];
          };
        };
      in
      {
        defaultPackage = self.packages.${system}.${packageName};
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            servant-auth-server =
              pkgs.lib.pipe haskellPackages.servant-auth-server
                [
                  pkgs.haskell.lib.unmarkBroken
                  pkgs.haskell.lib.dontCheck
                ];

            tomland =
              pkgs.lib.pipe haskellPackages.tomland
                [
                  pkgs.haskell.lib.doJailbreak
                  pkgs.haskell.lib.dontCheck
                ];
          };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              stylish-haskell.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };
        };

        devShells.default = pkgs.haskellPackages.shellFor rec {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          packages = p: [ self.packages.${system}.${packageName} ];

          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.hspec-discover
            haskellPackages.fourmolu
            hpack
            jq
            postgresql
            go-task
            toml2json
            watchexec
            zlib
          ];

          # Ensure that libz.so and other libraries are available to TH
          # splices, cabal repl, etc.
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
        };
      });
}
