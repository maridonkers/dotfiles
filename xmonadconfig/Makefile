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

lint:
	ag --haskell -l | xargs hlint -v

formatter:
	ag --haskell -l | xargs stylish-haskell -i

help:
	@grep '^[^ 	#:]\+:' Makefile | sed -e 's/:[^:]*//g'
