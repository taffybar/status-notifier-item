let
  pkgs = (import <nixpkgs> { });
  package = pkgs.haskellPackages.callCabal2nix "status-notifier-item" ./. { };
in pkgs.haskellPackages.shellFor { packages = _: [package]; }
