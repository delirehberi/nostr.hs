{
  description = "Nostr Haskell Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.${system} = rec {
      # Shell for routr-tracking-service (Cabal)
      nostrHs = pkgs.mkShell {
        buildInputs = with pkgs; [
          cabal-install
          haskell.compiler.ghc965
          haskellPackages.haskell-language-server
          haskellPackages.ghcid
          zlib
          zlib.dev
          pkg-config
          secp256k1
        ];

        shellHook = ''
          export PKG_CONFIG_PATH=${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH
          echo "Shell is ready"
        '';
      };

      default = nostrHs;
    };
  };
}

