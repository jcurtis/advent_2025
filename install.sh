set -ex

cabal install --lib --package-env . --avoid-reinstalls \
  split \
  text \
  extra
