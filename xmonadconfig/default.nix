# Required: cabal2nix
# To initialize project use the following:
#   nix-shell --packages ghc --run 'cabal init -p xmonadconfig'
# To enter a Nix-shell use the following (from within ~/.xmonad subdirectory; i.e. symbolic link):
#  nix-shell -A env
# (leave and re-enter when errors after .cabal file changed)
#
# To build use the following (from within ~/.xmonad):
#  cabal v2-build
#
# In Emacs use C-l to start GHCi; or use the following from command line:
#  cabal v2-repl
#
{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "xmonadconfig" ./. {}
