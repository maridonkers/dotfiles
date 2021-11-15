# This file is NixOS specific.
#
# Use make shell and subsequent make rebuild to check your source code.
# In Emacs accept .dir-locals.el prompt if you use Dante (dante-mode) and get errors.
#
{ pkgs ? import <nixpkgs> {} }:

(pkgs.haskellPackages.callCabal2nix "xmonadconfig" ./. {}).env
