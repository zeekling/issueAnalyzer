{
  description = "PyWebIO front-end development environment for Issue Browser";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = let
        pkgs = import nixpkgs { inherit system; };
      in pkgs.mkShell {
        buildInputs = [
          pkgs.python311Full
          pkgs.python311Packages.flask
          pkgs.python311Packages.pywebio
          pkgs.python311Packages.requests
        ];
        shellHook = ''
          echo "PyWebIO dev shell ready: run 'python api.py' (on port 8000)."
        '';
      };
    });
}
