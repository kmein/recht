{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nix "recht" ./. {
  blessings = pkgs.haskellPackages.callCabal2nix "blessings" (pkgs.fetchgit {
    url = "https://cgit.krebsco.de/blessings";
    rev = "v2.2.0";
    sha256 = "1pb56dgf3jj2kq3cbbppwzyg3ccgqy9xara62hkjwyxzdx20clk1";
  }) {};
}
