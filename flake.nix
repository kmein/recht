{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    blessings.url = "git+https://cgit.krebsco.de/blessings";
    blessings.flake = false;
  };


  outputs = inputs@{self, nixpkgs, flake-utils, blessings, ...}:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs {
      inherit system;
    };
  in {
    packages.recht = pkgs.haskellPackages.callCabal2nix "recht" self {
      blessings = pkgs.haskellPackages.callCabal2nix "blessings" blessings.outPath {};
    };
    defaultPackage = self.packages.${system}.recht;
  });
}
