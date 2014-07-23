{
  pkgs ? import <nixpkgs> {},
  src ? ./.
} : pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "nhc";
  cabalDrvArgs = {
    # The tests rely on 'nix-build' being in scope and
    # install things in the nix store. We don't want to
    # do that during compilation, so tests are disabled.
    doCheck = false;
  };
}
