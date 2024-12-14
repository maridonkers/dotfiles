all: help

xmonad:
	xmonad --recompile

# First use a make develop and then a make rebuild to check your
# changes. Finally a make xmonad to activate an updated configuration.
develop:
	nix-shell

edit:
	emacs &

rebuild: clean
	cabal new-build

clean:
	cabal new-clean

build: xmonad.hs
	cabal new-build
	ln -fs /home/mdo/xmonadconfig/dist-newstyle/build/x86_64-linux/ghc-9.4.8/xmonadconfig-0.1.0.0/x/xmonadconfig/build/xmonadconfig/xmonadconfig xmonad-x86_64-linux

lint:
	ag --haskell -l | xargs hlint -v

formatter:
	ag --haskell -l | xargs stylish-haskell -i

help:
	@grep '^[^ 	#:]\+:' Makefile | sed -e 's/:[^:]*//g'

