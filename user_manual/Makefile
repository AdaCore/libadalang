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

.PHONY: clean html generate generate-dev generate-prod laldoc-html

laldoc-html:
	python -m laldoc.generate_rst -P $(LAL_BUILD_DIR)/libadalang.gpr \
		$(LAL_BUILD_DIR)/src/libadalang-analysis.ads \
		../extensions/src/libadalang-auto_provider.ads \
		$(LAL_BUILD_DIR)/src/libadalang-common.ads \
		../extensions/src/libadalang-config_pragmas.ads \
		../extensions/src/libadalang-data_decomposition.ads \
		../extensions/src/libadalang-expr_eval.ads \
		$(LAL_BUILD_DIR)/src/libadalang-generic_api.ads \
		$(LAL_BUILD_DIR)/src/libadalang-generic_api-introspection.ads \
		$(LAL_BUILD_DIR)/src/libadalang-generic_api-unparsing.ads \
		../extensions/src/libadalang-helpers.ads \
		$(LAL_BUILD_DIR)/src/libadalang-iterators.ads \
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

	# eng/ide/gnatdoc#195: remove :defval: directives from the generated
	# Sphinx, as they don't bring anything valuable to the documentation, and
	# for this package, they even cause Sphinx issues because of the too long
	# lines.
	grep -v ':defval:' \
		generated/ada___libadalang__generic_api__introspection___spec.rst \
		> generated/tmp || true
	mv generated/tmp \
		generated/ada___libadalang__generic_api__introspection___spec.rst

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
