# nhc - simple build environments for cabal packages using nix

`nhc` allows to execute arbitrary commands inside a build environment
that is created from the cabal file. `nhc` allows you to do things like:

```
$ nhc cabal build
```

```
$ nhc hdevtools check src/Main.hs
```

Interactive commands are also allowed:

```
nhc ghci
```

```
nhc zsh
```

`nhc` provides basic development tools (`ghc`, `ghc-pkg`, `cabal`), but also
some convenient additional development tools. (Currently only `hdevtools` in
a version that tries to understand cabal files.)


# How it works

`nhc` is a thin wrapper around `nix-build` and some nix-expressions heavily
inspired by http://ocharles.org.uk/blog/posts/2014-02-04-how-i-develop-with-nixos.html.
So you need a working [nix](http://nixos.org/) installation for this to work.

Invoking `nhc` will not alter your user's nix profile (but it will install
software to the nix store). It creates two nix files `defaults.nix` and
`nhc.nix`. Then `nix-build` is invoked to create a script that sets up the wanted
environment. `nix-build` will put a link to the derivation containing the
script in `./result`. This script will normally drop you in a newly created
bash inside the created environment. `nhc` invokes this script to run its commands.

On subsequent invocations of `nhc` it will check whether the cabal file changed.
If it didn't, `nhc` will just execute the given command inside of the created
environment. If it did change, the new environment will be built (with `nix-build`),
and the given command will be executed in the new environment.


## hdevtools support

`hdevtools` is a little special since it spawns a background process to make
module checking fast. This background process runs in the environment that it
was started in, which only works as long as you don't rebuild your environment.
`nhc` therefore invokes `hdevtools --stop-server` when the environment is rebuilt.
