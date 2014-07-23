#!/usr/bin/env bash

set -o errexit

nix-build release.nix
if [ -e ./result ] ; then
  nix-env -i $(realpath result)
else
  echo ./result does not exist
  false
fi
