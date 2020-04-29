let

  pkgs = import ./nixpkgs.nix;
  ikproj-src = ./. ;

  server = pkgs.callPackage ./server/default.nix {};
  client = pkgs.callPackage ./client/default.nix {};

in pkgs.runCommand "ikproj" { inherit client server; } ''
  mkdir -p $out/{bin,static}
  cp ${server}/bin/* $out/bin/
  cp ${client}/static/* $out/static/
  cp ${ikproj-src}/assets/favicon.ico $out/static/
''
