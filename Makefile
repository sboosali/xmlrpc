##################################################
# Makefile Directives
##################################################

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables
##################################################

DefaultPackage=xmlrpc
DefaultModule=XmlRpc

DefaultLibraryTarget="lib:$(DefaultPackage)"
# e.g. "lib:xmlrpc"

PackageVersion=0.0.0

##################################################

CompilerFlavor=ghc

CompilerVersion=8.4.3

CompilerProgram=$(CompilerFlavor)-$(CompilerVersion)

##################################################

Cabal=cabal

Markdown=multimarkdown
#TODO pandoc

Open=xdg-open

##################################################

RootDirectory=$(CURDIR)

BuildDirectory=./dist-newstyle

ReleaseDirectory=./release
HaddockDirectory=$(ReleaseDirectory)/documentation
TarballDirectory=$(ReleaseDirectory)/tarballs

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
repl:
	$(Cabal) new-repl $(DefaultLibraryTarget)

.PHONY: repl

##################################################
test:
	$(Cabal) new-test $(DefaultLibraryTarget)

.PHONY: test

##################################################
clean:
	rm -rf "./dist/" "./dist-newstyle/" "./dist-newdante/"
	rm -f *.project.local .ghc.environment.*
	rm -rf "*/dist/" "*/dist-newstyle/"

.PHONY: clean

##################################################
# Building: different targets, compilers, build-tools.
##################################################

##################################################
build-default:
	$(Cabal) new-build $(DefaultLibraryTarget)

.PHONY: build-default

##################################################
cabal-compile:
	$(Cabal) new-build all

.PHONY: cabal-compile

##################################################
stack-compile:
	stack --nix build

.PHONY: stack-compile

##################################################
# Executables: building/running/registering them.
##################################################

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
# Documentation: building/copying/opening
##################################################

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
docs-haskell: build-docs-haskell copy-docs-haskell open-docs-haskell

.PHONY: docs-haskell

##################################################
build-docs-haskell: build-default
	@echo '=================================================='
	$(Cabal) new-haddock $(DefaultLibraryTarget) --enable-documentation
	@echo '=================================================='
	find $(BuildDirectory) -name "index.html" -print

.PHONY: build-docs-haskell

##################################################
copy-docs-haskell: build-docs-haskell
	mkdir -p $(HaddockDirectory)
	@echo '=================================================='
	cp -aRv  ./dist-newstyle/build/*-*/ghc-*/$(DefaultPackage)-$(PackageVersion)/doc/html/$(DefaultPackage)/src/* $(HaddockDirectory)

.PHONY: copy-docs-haskell

##################################################
open-docs-haskell: 
	@echo '=================================================='
	find $(HaddockDirectory) -name "$(DefaultModule).html" -print
	@echo '=================================================='
	find $(HaddockDirectory) -name "$(DefaultModule).html" -exec $(Open) \{\} \;

.PHONY: open-docs-haskell

##################################################
# Release: 
##################################################

##################################################
tarball: build
	@echo '=================================================='
	find $(RootDirectory) -name "*.cabal" -print0 | xargs -n 1 -0 $(Cabal) check
	@echo '=================================================='
	$(Cabal) sdist
	@echo '=================================================='
	mkdir -p $(TarballDirectory)
	find $(BuildDirectory) -name "*.tar.gz" -print0 -exec mv \{\} $(TarballDirectory) \;
	@echo '=================================================='
	find $(TarballDirectory) -name "*.tar.gz" -print0 | xargs -n 1 -0 $(Cabal) check
	@echo '=================================================='
#TODO move to

.PHONY: tarball

##################################################
upload: tarball
	$(Cabal) upload

.PHONY: upload

##################################################