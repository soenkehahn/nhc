
# It's not possible to run the test suite through nhc itself (because you can't
# execute nix-shell inside a nix-shell).  The test suite is disabled in
# 'release.nix' for that reason. This script can be used to run the test suite.

set -o errexit

nix-shell test.nix --pure --command "cabal clean && cabal configure --enable-tests"
nix-shell test.nix --pure --command "cabal build"

./dist/build/spec/spec --fail-fast
