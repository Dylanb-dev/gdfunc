{
  description = "Development environment for Haskell";

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
          ormolu  # formatter
          ghcid   # auto-recompiler
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
          buildInputs = haskellTools ++ commonTools;
          
          shellHook = ''
            echo "🚀 Development environment loaded!"
            echo ""
            echo "Haskell tools:"
            echo "  - GHC version: $(ghc --version)"
            echo "  - Cabal: $(cabal --version | head -n1)"
            echo ""
          '';
        };
      }
    );
}
