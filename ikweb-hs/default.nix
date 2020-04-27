let

  pkgs = import ./nixpkgs.nix;
  ikproj-src = ../. ;

  server = pkgs.callPackage ./server/default.nix {};
  client = pkgs.callPackage ./client/default.nix {};

in pkgs.runCommand "ikproj" { inherit client server; } ''
  mkdir -p $out/{bin,static}
  cp ${server}/bin/* $out/bin/
  ${pkgs.closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js
''
