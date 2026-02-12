{
  description = "status-notifier-item";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = github:hercules-ci/gitignore.nix/master;
  };
  outputs = {
    self,
    flake-utils,
    nixpkgs,
    git-ignore-nix,
  }: let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides =
          prev.lib.composeExtensions (old.overrides or (_: _: {}))
          (hself: hsuper: {
            status-notifier-item =
              prev.haskell.lib.overrideCabal
              (hself.callCabal2nix "status-notifier-item"
                (git-ignore-nix.lib.gitignoreSource ./.)
                {})
              (drv: {
                # Integration tests start an isolated D-Bus via `dbus-daemon`.
                testToolDepends = (drv.testToolDepends or []) ++ [final.dbus];
              });
          });
      });
    };
    overlays = [overlay];
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system overlays;
        config.allowBroken = true;
      };
    in rec {
      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [p.status-notifier-item];
        buildInputs = [
          pkgs.dbus
          pkgs.zlib.dev
        ];
      };
      defaultPackage = pkgs.haskellPackages.status-notifier-item;
    }) // {inherit overlay overlays;};
}
