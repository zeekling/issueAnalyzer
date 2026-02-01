{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.python311Full
    pkgs.python311Packages.flask
    pkgs.python311Packages.pywebio
    pkgs.python311Packages.requests
  ];
  shellHook = ''
    echo "PyWebIO dev shell ready. Run: python api.py"
  '';
}
