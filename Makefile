.PHONY: all clean sweep distclean rebuild serve
.DELETE_ON_ERROR:

THIS_MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(patsubst %/,%,$(dir $(THIS_MAKEFILE_PATH)))

# turn paths relative
RELIFY_CMD := perl -e 'use File::Spec; print File::Spec->abs2rel(@ARGV) . "\n"'

# npm
NODE_DIR := node_modules
NPM_BIN = $(shell npm bin)
COFFEE_CC = $(NPM_BIN)/coffee
UGLIFY_JS = $(NPM_BIN)/uglifyjs
UGLIFY_JS_OPTS := -mc --screw-ie8 2>/dev/null

NODE_DEPS = $(COFFEE_CC) $(UGLIFY_JS)

# cpan
HTML_PARSER = $(shell if perl -e 'use HTML::TokeParser::Simple'; then echo; \
	else echo "HTML_TokeParser_Simple"; fi) # blank if installed
PERL_DEPS = $(HTML_PARSER)

# submodules
HTMLIZE_DIR := htmlize
HTMLIZE_FILE := $(HTMLIZE_DIR)/htmlize.el
HTMLIZE_PROOF := $(HTMLIZE_DIR)/README.md
ORG_INFO_DIR := org-info-js
ORG_INFO_FILE := $(ORG_INFO_DIR)/org-info.js
ORG_STYLE_FILE := $(ORG_INFO_DIR)/stylesheet.css
ORG_INFO_PROOF := $(ORG_INFO_DIR)/README.md

SUBMODULES := $(HTMLIZE_DIR) $(ORG_INFO_DIR)
SUBMODULE_PROOFS := $(HTMLIZE_PROOF) $(ORG_INFO_PROOF)

# build scripts
SETUP_DIR := $(CURRENT_DIR)/setup
DEPS := $(SUBMODULE_PROOFS) $(NODE_DEPS) $(PERL_DEPS) \
	$(wildcard $(SETUP_DIR)/*) $(THIS_MAKEFILE_PATH)

# we read from this to setup everything else
JEKYLL_CONFIG := _config.yml

SWITCH_DIR_SCRIPT := $(SETUP_DIR)/switch-dir.sh

OUT_DIR := $(CURRENT_DIR)/cosmicexplorer.github.io
ORG_PATTERN := -type f -name "*.org" \
	$(patsubst %,-not -iwholename "*%/*", $(ORG_INFO_DIR) $(OUT_DIR))
ORG_DIR := $(CURRENT_DIR)
ORG_IN := $(shell find $(ORG_DIR) $(ORG_PATTERN) | sort | uniq)
OUT_PAGES := $(patsubst %.org, %.html, \
	$(shell $(SWITCH_DIR_SCRIPT) $(CURRENT_DIR) $(OUT_DIR) $(ORG_IN)))

SCRIPTS_DIR := scripts
OUT_SCRIPTS_DIR := $(OUT_DIR)/$(SCRIPTS_DIR)
OUT_SCRIPTS := $(patsubst $(SCRIPTS_DIR)/%.coffee,$(OUT_SCRIPTS_DIR)/%.js, \
	$(shell find $(SCRIPTS_DIR) -name "*.coffee"))

STYLES_DIR := styles
OUT_STYLES_DIR := $(OUT_DIR)/$(STYLES_DIR)
OUT_STYLES :=

# now htmlize every source file, and copy other html files
GIT_DIR := .git
EXCL_FILE_PATTERN := \( -name "\#*" -or -name "*\~" -or -name ".\#*" \)
PROJ_FILE_PATTERN := -type f -not $(EXCL_FILE_PATTERN) \
	$(patsubst %,-not -iwholename "*%/*", $(GIT_DIR) $(NODE_DIR) \
		$(OUT_DIR) $(ORG_INFO_DIR) $(HTMLIZE_DIR))

HTMLIZE_PATTERN := $(PROJ_FILE_PATTERN) -not -name "*.html"
HTMLIZE_IN := $(shell find $(CURRENT_DIR) $(HTMLIZE_PATTERN))
HTMLIZE_OUT := $(patsubst %,%.html, \
	$(shell $(SWITCH_DIR_SCRIPT) $(CURRENT_DIR) $(OUT_DIR) \
		$(HTMLIZE_IN)))

COPY_PATTERN := $(PROJ_FILE_PATTERN) -name "*.html"
COPY_IN := $(shell find $(CURRENT_DIR) $(COPY_PATTERN))
COPY_OUT := $(shell $(SWITCH_DIR_SCRIPT) $(CURRENT_DIR) $(OUT_DIR) $(COPY_IN))

ORG_INFO_OUT_DIR := $(OUT_DIR)/$(ORG_INFO_DIR)
ORG_INFO_OUT := $(ORG_INFO_OUT_DIR)/stylesheet-mini.css \
	$(ORG_INFO_OUT_DIR)/org-info-mini.js

OUT_DIRS := $(OUT_SCRIPTS_DIR) $(OUT_STYLES_DIR) $(ORG_INFO_OUT_DIR)

all: $(OUT_PAGES) $(OUT_SCRIPTS) $(OUT_STYLES) $(HTMLIZE_OUT) \
	$(HTMLIZE_MAKEFILE) $(COPY_OUT) $(ORG_INFO_OUT) | $(OUT_DIRS)

$(OUT_DIRS):
	mkdir -p $(shell $(RELIFY_CMD) $@)

# scripts
$(OUT_SCRIPTS_DIR)/%.js: $(SCRIPTS_DIR)/%.coffee | $(OUT_SCRIPTS_DIR)
	@echo "$< => $(shell $(RELIFY_CMD) $@)"
	@$(COFFEE_CC) -bcp --no-header $< | $(UGLIFY_JS) $(UGLIFY_JS_OPTS) > $@
$(OUT_SCRIPTS_DIR)/%.js: $(ORG_INFO_DIR)/%-mini.js
	@echo "$< => $@"
	@cp $< $@

# styles
$(OUT_STYLES_DIR)/%.css: $(STYLES_DIR)/%.sass
	@echo "$< => $@"
	@exit 1
$(OUT_STYLES_DIR)/%.css: $(ORG_INFO_DIR)/%-mini.css
	@echo "$< => $@"
	@cp $< $@

# make the minified versions
$(ORG_INFO_OUT): $(ORG_INFO_PROOF) $(ORG_INFO_OUT_DIR)
	@$(MAKE) -C $(ORG_INFO_DIR)
	@echo "[" $(ORG_INFO_DIR)/*-mini* "] =>" \
		"($(shell $(RELIFY_CMD) $(ORG_INFO_OUT_DIR)))"
	@cp $(ORG_INFO_DIR)/*-mini* $(ORG_INFO_OUT_DIR)

# make html from org
MIGRATE_SCRIPT := $(SETUP_DIR)/migrate-org.el
$(OUT_PAGES): $(ORG_IN) $(DEPS)
	@$(MIGRATE_SCRIPT) $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $(ORG_IN) \
		1>&2 2>/dev/null

# htmlize
HTMLIZE_TMP_FILE := $(SETUP_DIR)/tmpfile
HTMLIZE_SCRIPT := $(SETUP_DIR)/htmlize-file.sh \
	$(shell cat $(CURRENT_DIR)/.xvfb.config) $(HTMLIZE_TMP_FILE)
HTMLIZE_OUT_FILE := $(SETUP_DIR)/output-file
$(HTMLIZE_OUT): $(HTMLIZE_IN)
	@for el in $(HTMLIZE_OUT_FILE) $(CURRENT_DIR)/$(HTMLIZE_FILE) \
		$(ORG_DIR) $(OUT_DIR) $(HTMLIZE_IN); \
		do echo "$$el"; done > $(HTMLIZE_TMP_FILE)
	@$(HTMLIZE_SCRIPT) 2>/dev/null
	@find . $(EXCL_FILE_PATTERN) -exec rm '{}' ';'

# create submodules and dependent packages
$(SUBMODULE_PROOFS):
	@git submodule update --init --recursive
$(NODE_DEPS):
	@npm install

$(HTML_PARSER):
	@cpan HTML::TokeParser::Simple

sweep:
	@find . $(EXCL_FILE_PATTERN) -exec rm '{}' ';'

clean: $(DEPS) sweep
	@$(MAKE) -C $(ORG_INFO_DIR) clean
	@$(MAKE) -C $(OUT_DIR) clean
	@rm -f $(HTMLIZE_TMP_FILE)

distclean: clean
	@rm -rf $(NODE_DIR)
	@git submodule deinit -f $(CURRENT_DIR)

rebuild:
	$(MAKE) clean
	$(MAKE) all
