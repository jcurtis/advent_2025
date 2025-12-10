set -ex

cabal install --lib --package-env . --force-reinstalls \
  split \
  text \
  extra \
  containers
