{
  description = "Issue Analyzer Flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: {
      pkgs = import nixpkgs { inherit system; };

      packages = {
        server = pkgs.stdenv.mkDerivation {
          pname = "issue-analyzer-server";
          version = "1.0.0";
          src = "."; # use repo contents as source
          buildInputs = [
            pkgs.python311Full
            pkgs.python311Packages.flask
            pkgs.python311Packages.pywebio
          ];
          installPhase = ''
            mkdir -p $out/bin
            # Copy repo contents into output for runtime availability
            cp -r ${src}/* $out/
            cat > $out/bin/issue-analyzer-server <<'EOS'
            #!/usr/bin/env bash
            SCRIPT_DIR=$(cd -- "$(dirname "$0")" && pwd)
            SCRIPT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)
            exec python3 "$SCRIPT_ROOT/api.py"
            EOS
            chmod +x $out/bin/issue-analyzer-server
          '';
          meta = with pkgs.lib; {
            description = "Flask server for Issue Analyzer with PyWebIO frontend";
            license = licenses.mit;
          };
        };
      };

      defaultPackage = self.packages.server;
    });
}
