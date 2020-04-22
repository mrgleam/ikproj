let

  pkgs = import ./nixpkgs.nix;
  server = pkgs.callPackage ./server/default.nix {};
  client = pkgs.callPackage ./client/default.nix {};

in pkgs.runCommand "ikproj" { inherit client server; } ''
  mkdir -p $out/bin/static
  cp ${server}/bin/* $out/bin/
  cp ${client}/static/* $out/bin/static/
''
