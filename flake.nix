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
          packages = (with pkgs; [
            cairo
            expat
            libglvnd
            libGLU
            libsysprof-capture
            pango
            pcre2
            pkg-config
            util-linux
            libselinux
            libsepol
            fribidi
            libthai
            libdatrie
          ]) ++ (with pkgs.xorg; [
            libXinerama
            libXrandr
            libXi
            libXxf86vm
            libXcursor
            libXdmcp
            libxcb
          ]);
        };
        apps = rec {
          gl-plot = flake-utils.lib.mkApp { drv = self.packages.${system}.gl-plot; };
          default = gl-plot;
        };
      }
    );
}
