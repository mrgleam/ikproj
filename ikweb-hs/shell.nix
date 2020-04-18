let

  pkgs = import ./nix/nixpkgs.nix;

  server = pkgs.haskell.packages.ghc.callCabal2nix "ikproj" ./. { };
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "ikproj" ./. { };

in {
  server = server.env;
  client = client.env;
}

