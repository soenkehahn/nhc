nhc
===

`nhc` combines `hdevtools` (http://hackage.haskell.org/package/hdevtools)
with nix (http://nixos.org/). It currently acts as a drop-in replacement
for `hdevtools`, but it makes sure `hdevtools` is executed in an environment
where `ghc` and all cabal dependencies are already installed (through nix).
Invoking `nhc` will not alter your user's nix profile (but it will install
software in the nix store).

How it works
============

It creates two nix files `defaults.nix` and `nhc.nix`, the latter being
heavily inspired by
http://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html.
Then `nix-build` is invoked to create a script that sets up the wanted
environment. `nix-build` will put a link to the derivation containing the
script in `./result`. This script will normally drop you in a newly created
bash inside the created environment. `nhc` pipes `hdevtools ARGS` into this
script.

On subsequent invocations of `nhc` it will look whether the cabal file changed.
If it didn't, `nhc` will execute `hdevtools` inside of the created environment
again. If it did change, the new environment will be build (with `nix-build`)
and the `hdevtools` background process (that is still running in the old
environment) will be terminated. A new call to `hdevtools ARGS` in the new
environment is performed.

You can also use `./result/bin/load-env-nhc-build` manually to enter the created
environment.
