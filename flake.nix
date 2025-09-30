{
  description = "gl-plot";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        packages = rec {
          gl-plot = pkgs.haskellPackages.callCabal2nix "gl-plot" ./. {};
          default = gl-plot;
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.gl-plot ];
          packages = with pkgs; [
            pango
            cairo
            pkg-config
          ];
        };
        apps = rec {
          gl-plot = flake-utils.lib.mkApp { drv = self.packages.${system}.gl-plot; };
          default = gl-plot;
        };
      }
    );
}
