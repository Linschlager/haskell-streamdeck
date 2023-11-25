{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-filter.url = "github:numtide/nix-filter";

  outputs = inputs:
    let
      lib = inputs.nixpkgs.lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else error "foreach: expected list or attrset"
      );
      pname = "streamdeck";
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "lib"
          "${pname}.cabal"
          "CHANGELOG.md"
          "LICENCE"
        ];
      };
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs:
      let
        defaultGhc = builtins.replaceStrings [ "." "-" ] [ "" "" ] pkgs.haskellPackages.ghc.name;
      in
      lib.recursiveUpdate
        {
          packages.${system}.default = inputs.self.packages.${system}."${pname}-${defaultGhc}";
          devShells.${system}.default = inputs.self.devShells.${system}.${defaultGhc};
          formatter.${system} = pkgs.nixpkgs-fmt;
        }
        (foreach (lib.filterAttrs (name: _: builtins.match "ghc[0-9]+" name != null) pkgs.haskell.packages)
          (ghcName: haskellPackages:
            let
              hp = haskellPackages.override {
                overrides = self: super: with pkgs.haskell.lib.compose; {
                  hidapi =
                    if (system == "x86_64-darwin")
                    then
                      overrideCabal
                        (drv: {
                          extraLibraries = [ ];
                          librarySystemDepends = [
                            pkgs.darwin.apple_sdk.frameworks.AppKit
                          ];
                        })
                        (
                          (super.hidapi.override { systemd = null; }).overrideAttrs (attrs: { meta = attrs.meta // { badPlatforms = [ ]; }; }))
                    else super.hidapi;
                  "${pname}" = super.callCabal2nix pname src { };
                };
              };
            in
            {
              packages.${system}."${pname}-${ghcName}" = hp.${pname};
              devShells.${system}.${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ];
                nativeBuildInputs = with hp; [
                  cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            }
          )
        ));
}
