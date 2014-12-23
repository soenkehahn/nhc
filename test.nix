{
  pkgs ? import <nixpkgs> {},
  src ? builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist" &&
    baseNameOf path != ".nhc") ./.
} : pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "nhc";
  cabalDrvArgs = {
    doCheck = true;
  };
}
