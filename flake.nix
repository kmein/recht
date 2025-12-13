{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    blessings.url = "git+https://cgit.krebsco.de/blessings";
    blessings.flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      blessings,
    }:
    let
      lib = nixpkgs.lib;
      eachSupportedSystem = lib.genAttrs lib.systems.flakeExposed;
      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
        };
    in
    {
      packages = eachSupportedSystem (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          recht = pkgs.haskellPackages.callCabal2nix "recht" self {
            blessings = pkgs.haskellPackages.callCabal2nix "blessings" blessings.outPath { };
          };
        }
      );
      defaultPackage = eachSupportedSystem (system: self.packages.${system}.recht);
    };
}
