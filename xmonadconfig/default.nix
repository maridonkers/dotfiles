# This file is NixOS specific.
#
# Required: cabal2nix
# To initialize project --which has already been done here, so don't-- use the following:
#   nix-shell --packages ghc --run 'cabal init -p xmonadconfig'
#
# To enter a Nix-shell use the following (from within ~/.xmonad subdirectory; i.e. symbolic link):
#  nix-shell -A env  (leave and re-enter when errors after .cabal file changed)
#
# To build use the following (from within ~/.xmonad):
#  cabal v2-build
#
# In Emacs use C-l to start GHCi; or use the following from command line:
#  cabal v2-repl
#
# In Emacs accept .dir-locals.el prompt if you use Dante (dante-mode) and get errors.
#
{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "xmonadconfig" ./. {}
