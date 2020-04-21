{ pkgs ? import <nixpkgs> { } }:

let

  ikproj = import ./default.nix;

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}
    $@
  '';

in pkgs.dockerTools.buildImage {
  name = "ikproj";
  tag = "latest";
  created = "now";
  config = {
    WorkingDir = "${ikproj}";
    Entrypoint = [ entrypoint ];
    Cmd = [ "${ikproj}/bin/server" ];
  };
}

