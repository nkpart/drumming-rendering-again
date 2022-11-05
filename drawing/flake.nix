{
  description = "";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "aarch64-darwin"]
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in 
        {
          devShell = with pkgs; mkShell {
            buildInputs = [
              (haskellPackages.ghcWithPackages (pkgs: [ pkgs.diagrams ]))
              haskell-language-server
              ghcid
              cabal-install
              hpack
              bashInteractive
            ];

            #shellHook = ''
            #'';
            shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
          };
        }
      );
}