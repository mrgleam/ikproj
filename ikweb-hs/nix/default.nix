let

  pkgs = import ./nixpkgs.nix;
  ikproj-src = ../.;

  server = pkgs.haskell.packages.ghc.callCabal2nix "ikproj" ../. { };
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "ikproj" ../. { };

in pkgs.runCommand "ikproj" { inherit client server; } ''
  mkdir -p $out/{bin,static}
  cp ${server}/bin/* $out/bin/
  ${pkgs.closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js
  cp ${miso-xhr-src}/static/*.png $out/static/
''

