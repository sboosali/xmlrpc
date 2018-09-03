##################################################
default: build

.PHONY: default

##################################################
check:
	cabal new-build -fno-code -O0 all

.PHONY: check

##################################################
repl:
	cabal new-repl haxr

.PHONY: repl

##################################################
clean:
	rm -rf "dist/" "dist-newstyle/"
	rm -f *.project.local .ghc.environment.*

.PHONY: clean

##################################################
build: cabal-compile

.PHONY: build

##################################################
cabal-compile:
	cabal new-build all

.PHONY: cabal-compile

##################################################
stack-compile:
	stack --nix build

.PHONY: stack-compile

##################################################
examples: build
	@echo '=================================================='
	cabal new-run xmlrpc-example-time
	@echo '=================================================='
	cabal new-run xmlrpc-example-validator
	@echo '=================================================='
	cabal new-run xmlrpc-example-introspect
	@echo '=================================================='
	cabal new-run xmlrpc-example-simple
	@echo '=================================================='
	cabal new-run xmlrpc-example-person
	@echo '=================================================='

.PHONY: examples

##################################################
sdist: build
	cabal sdist

.PHONY: sdist

##################################################