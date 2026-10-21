# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = _build
LAL_BUILD_DIR = ../build
LAL_GPR_DIR   = $(LAL_BUILD_DIR)
LANGKIT_DIR   = ../../langkit

# Designates the langkit_support directory. This is only used for production
# builds (generate-prod target) but set to the source directory below by
# default so that production target can still be run/tested locally.
LANGKIT_SUPPORT_GPR_DIR = $(LANGKIT_DIR)/langkit/support

# Internal variables.
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

.PHONY: clean html generate generate-dev generate-prod

# When generating documentation, gnatdoc limits its analysis to the units
# covered by the given project and all of its dependencies, excluding those
# marked as Externally_Built. Running gnatdoc in the development environment is
# therefore enough to gather langkit_support's documentation. In our production
# environment, however, gnatdoc runs on the installed packages, which are marked
# as Externally_Built. Hence the two targets below, which together produce the
# full documentation in either environment.
generate-dev:
	mkdir -p external
	cp $(LANGKIT_DIR)/langkit/support/unparsing_config_format.rst external
	gnatdoc --backend rstpt -P $(LAL_GPR_DIR)/libadalang.gpr --generate public -O generated

generate-prod: generate-dev
	gnatdoc --backend rstpt -P $(LANGKIT_SUPPORT_GPR_DIR)/langkit_support.gpr --generate public -O generated

ifeq ($(LAL_GPR_DIR),$(LAL_BUILD_DIR))
generate: generate-dev
else
generate: generate-prod
endif

html: generate
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

clean:
	rm -rf external/*
	rm -rf generated/*
	rm -rf $(BUILDDIR)/*
