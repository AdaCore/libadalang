# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = _build
LAL_BUILD_DIR = ../build
LANGKIT_DIR = ../../langkit

# Internal variables.
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

.PHONY: clean html newhtml

newhtml:
	python -m laldoc.generate_rst -P $(LAL_BUILD_DIR)/libadalang.gpr \
		$(LAL_BUILD_DIR)/src/libadalang-analysis.ads \
		../extensions/src/libadalang-auto_provider.ads \
		$(LAL_BUILD_DIR)/src/libadalang-common.ads \
		../extensions/src/libadalang-config_pragmas.ads \
		../extensions/src/libadalang-data_decomposition.ads \
		../extensions/src/libadalang-expr_eval.ads \
		../extensions/src/libadalang-helpers.ads \
		$(LAL_BUILD_DIR)/src/libadalang-iterators.ads \
		$(LAL_BUILD_DIR)/src/libadalang-lexer.ads \
		../extensions/src/libadalang-preprocessing.ads \
		../extensions/src/libadalang-project_provider.ads \
		../extensions/src/libadalang-target_info.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-diagnostics.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-diagnostics-output.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-errors.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-file_readers.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-generic_api.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-generic_api-analysis.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-generic_api-introspection.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-generic_api-rewriting.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-generic_api-unparsing.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-names.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-names-maps.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-slocs.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-symbols.ads \
		$(LANGKIT_DIR)/langkit/support/langkit_support-text.ads \
		-O generated

	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) -t new_laldoc $(BUILDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

clean:
	rm -rf generated/*
	rm -rf $(BUILDDIR)/*

html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) -t legacy_laldoc $(BUILDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."
