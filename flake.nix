{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "aarch64-darwin" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        haskell-hello = final.haskellPackages.callCabal2nix "haskell-hello" ./. {};

        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (hself: hsuper: {
            list-zipper = with final.haskell.lib; markUnbroken (doJailbreak hsuper.list-zipper);
          });
        });
      });
      packages = forAllSystems (system: {
         haskell-hello = nixpkgsFor.${system}.haskell-hello;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.haskell-hello);
      checks = self.packages;
      devShell = forAllSystems (system: 
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
            allPkgs = nixpkgsFor.${system};
            lily = (allPkgs.writeScriptBin "lilypond" ''
             #!${allPkgs.stdenv.shell}
             # For lilypond, prevents "Fontconfig error: Cannot load default config file" and segfault
             export FONTCONFIG_FILE=${
             allPkgs.makeFontsConf {
               fontDirectories = [ "/Library/Fonts" "~/Library/Fonts" "/System/Library/Fonts" ];
               }
             }
             exec ${allPkgs.lilypond}/bin/lilypond $@
             '');
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.haskell-hello];
          withHoogle = true;

          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            hpack
            lily
            allPkgs.bashInteractive

            ghci-dap haskell-debug-adapter
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
