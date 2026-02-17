{
  description = "Nostr Haskell Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    haskellPackages = pkgs.haskell.packages.ghc982; # GHC 9.8.2 for better compatibility
  in {
    devShells.${system} = rec {
      # Shell for routr-tracking-service (Cabal)
      nostrHs = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskellPackages.ghc # Use GHC 9.10.1
          cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.haskell-dap
          haskellPackages.ghci-dap
          haskellPackages.haskell-debug-adapter
          pkg-config
          secp256k1
          zlib 
          zlib.dev 
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

