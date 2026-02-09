{
  description = "Development environment for Haskell and C compilation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Haskell packages
        haskellPackages = pkgs.haskellPackages;
      
        # Haskell tools
        haskellTools = with haskellPackages; [
          ghc
          cabal-install
          haskell-language-server
          hlint
          ormolu
          ghcid
        ];
        
        # Minimal C toolchain
        cTools = with pkgs; [
          gcc
          gnumake
        ];
        
        # C libraries needed by Haskell packages
        cLibs = with pkgs; [
          zlib        # Required by many Haskell packages
          zlib.dev    # Development headers
        ];
        
        # Common development tools
        commonTools = with pkgs; [
          git
          direnv
          nixpkgs-fmt
        ];
        
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = haskellTools ++ cTools ++ cLibs ++ commonTools;
          
          shellHook = ''
            echo "🚀 Development environment loaded!"
            echo ""
            echo "Haskell:"
            echo "  - GHC: $(ghc --version)"
            echo "  - Cabal: $(cabal --version | head -n1)"
            echo ""
            echo "C toolchain:"
            echo "  - GCC: $(gcc --version | head -n1)"
            echo ""
          '';
        };
      }
    );
}