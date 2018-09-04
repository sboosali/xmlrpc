##################################################
# Makefile Directives
##################################################

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables
##################################################

DefaultTarget="lib:xmlrpc"

BaseDirectory=$(CURDIR)

##################################################

Cabal=cabal

Markdown=multimarkdown
#TODO pandoc

##################################################
# the `default` target
##################################################

default: build

.PHONY: default

##################################################
# `cabal` wrapper targets
##################################################

check:
	$(Cabal) new-build -fno-code -O0 all

.PHONY: check

##################################################
build: build-default

.PHONY: build

##################################################
build-default:
	$(Cabal) new-build $(DefaultTarget)

.PHONY: build-default

##################################################
repl:
	$(Cabal) new-repl $(DefaultTarget)

.PHONY: repl

##################################################
test:
	$(Cabal) new-test $(DefaultTarget)

.PHONY: test

##################################################
clean:
	rm -rf "dist/" "dist-newstyle/"
	rm -f *.project.local .ghc.environment.*

.PHONY: clean

##################################################
cabal-compile:
	$(Cabal) new-build all

.PHONY: cabal-compile

##################################################
stack-compile:
	stack --nix build

.PHONY: stack-compile

##################################################
build-examples:
	$(Cabal) new-build xmlrpc-examples

.PHONY: build-examples

##################################################
examples: build-examples
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-time
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-validator
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-introspect
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-simple
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-person
	@echo '=================================================='

.PHONY: examples

##################################################
docs: docs-all

.PHONY: docs

##################################################
docs-all: docs-markdown docs-haskell

.PHONY: docs-all

##################################################
docs-markdown: 
	find . -name '*.md'   -print0 | xargs -n 1 -0 $(Markdown)
	find . -name '*.html' -print0

#TODO $(Markdown) 1.md > 1.html
#
# currently, it only checks the `md`.
#
#	find . -name '*.md' -exec sh -c '"$(Markdown)" "$1"' _  \{\} \;
#
#	find . -name '*.md' -print0 | xargs -n 1 -0 $(Markdown)

.PHONY: docs-markdown

# ^ 
# https://stackoverflow.com/questions/15030563/redirecting-stdout-with-find-exec-and-without-creating-new-shell

##################################################
docs-haskell: 
	$(Cabal) new-haddock all

.PHONY: docs-haskell

##################################################
sdist: build
	$(Cabal) sdist

.PHONY: sdist

##################################################