.PHONY: all clean sweep distclean rebuild serve HTML_TokeParser_Simple
.DELETE_ON_ERROR:

# utilities
THIS_MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(patsubst %/,%,$(dir $(THIS_MAKEFILE_PATH)))
# turn paths relative
RELIFY_CMD := perl -e 'use File::Spec; print File::Spec->abs2rel(@ARGV) . "\n"'
ABSOLUTIFY_CMD := readlink -f

# config file
CFG := site.config
SETUP_DIR := $(CURRENT_DIR)/setup
QUERY_CFG_CMD := $(SETUP_DIR)/parse_config.sh site.config
EXCEPT := $(SETUP_DIR)/except.sh

# npm
NODE_DIR := node_modules
NPM_BIN = $(shell npm bin)
COFFEE_CC = $(NPM_BIN)/coffee
UGLIFY_JS = $(NPM_BIN)/uglifyjs
UGLIFY_JS_OPTS := -mc --screw-ie8 2>/dev/null
UGLIFY_CSS = $(NPM_BIN)/uglifycss
BROWSERIFY_CC = $(NPM_BIN)/browserify

SCRIPTS_DIR := scripts
HIGHLIGHT_JS := highlight.js
BROWSERIFY_CONVERSIONS := $(HIGHLIGHT_JS)
BROWSERIFY_BUNDLE := $(SCRIPTS_DIR)/bundle.js

NODE_DEPS = $(COFFEE_CC) $(UGLIFY_JS) $(UGLIFY_CSS) $(BROWSERIFY_CC) \
	$(patsubst %,$(NODE_DIR)/%,$(BROWSERIFY_CONVERSIONS))

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

ORG_MODE_DIR := org-mode
ORG_MODE_PROOF := $(ORG_MODE_DIR)/README
ORG_PUB_FILE := $(ORG_MODE_DIR)/lisp/ox-publish.el
ORG_LOADDEFS := $(ORG_MODE_DIR)/lisp/org-loaddefs.el

SUBMODULES := $(HTMLIZE_DIR) $(ORG_LOADDEFS) $(ORG_INFO_DIR) $(ORG_MODE_DIR)
SUBMODULE_PROOFS := $(HTMLIZE_PROOF) $(ORG_INFO_PROOF) $(ORG_MODE_PROOF)

# build scripts
DEPS := $(SUBMODULE_PROOFS) $(NODE_DEPS) $(PERL_DEPS) \
	$(wildcard $(SETUP_DIR)/*) $(THIS_MAKEFILE_PATH) $(CFG)

SWITCH_DIR_SCRIPT := $(SETUP_DIR)/switch-dir.sh

OUT_DIR := $(shell $(ABSOLUTIFY_CMD) $(shell $(QUERY_CFG_CMD) outdir))
ORG_PATTERN := -type f -name "*.org"
IN_DIR := $(shell $(ABSOLUTIFY_CMD) $(shell $(QUERY_CFG_CMD) indir))
ORG_IN := $(shell find $(IN_DIR) $(ORG_PATTERN) | sort | uniq | \
		$(EXCEPT) $(shell find $(CURRENT_DIR) $(ORG_PATTERN)))
OUT_PAGES := $(patsubst %.org, %.html, \
	$(shell $(SWITCH_DIR_SCRIPT) $(IN_DIR) $(OUT_DIR) $(ORG_IN)))

OUT_SCRIPTS_DIR := $(OUT_DIR)/$(SCRIPTS_DIR)
OUT_SCRIPTS := $(patsubst $(SCRIPTS_DIR)/%.coffee,$(OUT_SCRIPTS_DIR)/%.js, \
	$(shell find $(SCRIPTS_DIR) -name "*.coffee")) \
	$(patsubst $(SCRIPTS_DIR)/%,$(OUT_SCRIPTS_DIR)/%,$(BROWSERIFY_BUNDLE))

STYLES_DIR := styles
OUT_STYLES_DIR := $(OUT_DIR)/$(STYLES_DIR)
OUT_STYLES := $(patsubst $(STYLES_DIR)/%,$(OUT_STYLES_DIR)/%, \
	$(wildcard $(STYLES_DIR)/*.css))

# now htmlize every source file, and copy other html files
GIT_DIR := .git
EXCL_FILE_PATTERN := \( -name "\#*" -or -name "*\~" -or -name ".\#*" \)
SPECIAL_FILE_PATTERN := -name ".git*"
PROJ_FILE_PATTERN := -type f -not $(EXCL_FILE_PATTERN) \
	-not $(SPECIAL_FILE_PATTERN) \
	$(patsubst %,-not -iwholename "*%/*", $(GIT_DIR) $(NODE_DIR) \
		$(ORG_INFO_DIR) $(HTMLIZE_DIR) $(CURRENT_DIR) \
		$(OUT_SCRIPTS_DIR) $(OUT_STYLES_DIR) $(ORG_INFO_OUT_DIR))

HTMLIZE_PATTERN := $(PROJ_FILE_PATTERN) -not -name "*.html"
HTMLIZE_IN := $(shell find $(IN_DIR) $(HTMLIZE_PATTERN))
HTMLIZE_OUT := $(patsubst %,%.html, \
	$(shell $(SWITCH_DIR_SCRIPT) $(IN_DIR) $(OUT_DIR) \
		$(HTMLIZE_IN)))

COPY_EXCL_FILE := \( -name "Makefile" -or -name ".git*" \)
COPY_PATTERN := $(PROJ_FILE_PATTERN) -not -name "*.html" -not $(COPY_EXCL_FILE)
COPY_IN := $(shell find $(IN_DIR) $(COPY_PATTERN))
COPY_OUT := $(shell $(SWITCH_DIR_SCRIPT) $(IN_DIR) $(OUT_DIR) $(COPY_IN) | \
	$(EXCEPT) $(COPY_IN))

ORG_INFO_OUT_DIR := $(OUT_DIR)/$(ORG_INFO_DIR)
ORG_INFO_OUT := $(ORG_INFO_OUT_DIR)/stylesheet-mini.css \
	$(ORG_INFO_OUT_DIR)/org-info-mini.js

OUT_DIRS := $(OUT_SCRIPTS_DIR) $(OUT_STYLES_DIR) $(ORG_INFO_OUT_DIR)

all: $(OUT_PAGES) $(OUT_SCRIPTS) $(OUT_STYLES) $(HTMLIZE_OUT) \
	$(HTMLIZE_MAKEFILE) $(COPY_OUT) $(ORG_INFO_OUT) | $(OUT_DIRS)

$(OUT_DIRS):
	mkdir -p $(shell $(RELIFY_CMD) $@)

# scripts
$(OUT_SCRIPTS_DIR)/%.js: $(SCRIPTS_DIR)/%.coffee $(COFFEE_CC) \
		| $(OUT_SCRIPTS_DIR)
	@echo "$< => $(shell $(RELIFY_CMD) $@)"
	@$(COFFEE_CC) -bcp --no-header $< | $(UGLIFY_JS) $(UGLIFY_JS_OPTS) > $@
$(OUT_SCRIPTS_DIR)/%.js: $(ORG_INFO_DIR)/%-mini.js | $(OUT_SCRIPTS_DIR)
	@echo "$< => $@"
	@cp $< $@
$(OUT_SCRIPTS_DIR)/%: $(SCRIPTS_DIR)/% | $(OUT_SCRIPTS_DIR)
	@echo "$< => $(shell $(RELIFY_CMD) $@)"
	@cp $< $@

# styles
$(OUT_STYLES_DIR)/%.css: $(STYLES_DIR)/%.css | $(OUT_STYLES_DIR)
	@echo "$< => $(shell $(RELIFY_CMD) $@)"
	@$(UGLIFY_CSS) $< > $@
$(OUT_STYLES_DIR)/%.css: $(ORG_INFO_DIR)/%-mini.css | $(OUT_SCRIPTS_DIR)
	@echo "$< => $@"
	@cp $< $@

# make the minified versions (note that these are all copied as a batch)
$(ORG_INFO_OUT): $(ORG_INFO_PROOF) $(ORG_INFO_OUT_DIR)
	@$(MAKE) -C $(ORG_INFO_DIR)
	@for el in $(ORG_INFO_DIR)/*-mini*; do \
		echo "$$el => $$($(RELIFY_CMD) $$($(SWITCH_DIR_SCRIPT) \
			$(ORG_INFO_DIR) $(ORG_INFO_OUT_DIR) $$el))"; done
	@cp $(ORG_INFO_DIR)/*-mini* $(ORG_INFO_OUT_DIR)

# make html from org
MIGRATE_SCRIPT := $(SETUP_DIR)/migrate-org.el
DO_EXPORT_EMAIL := $(shell $(QUERY_CFG_CMD) export_email || echo y)
DO_HL_CSS := $(shell $(QUERY_CFG_CMD) highlight_css || echo n)
DO_ORG_INFO := $(shell $(QUERY_CFG_CMD) org_info || echo y)
$(OUT_PAGES): $(ORG_IN) $(DEPS) $(ORG_LOADDEFS)
	@$(MIGRATE_SCRIPT) $(HTMLIZE_FILE) $(ORG_PUB_FILE) $(IN_DIR) $(OUT_DIR) \
		$(DO_EXPORT_EMAIL) $(DO_HL_CSS) $(DO_ORG_INFO) $(ORG_IN) \
		1>&2 2>/dev/null

# htmlize
HTMLIZE_TMP_FILE := $(SETUP_DIR)/tmpfile
HTMLIZE_SCRIPT := $(SETUP_DIR)/htmlize-file.sh $(HTMLIZE_TMP_FILE) \
	$(shell $(QUERY_CFG_CMD) xvfb_disp)
HTMLIZE_OUT_FILE := $(SETUP_DIR)/output-file
# '-' added so the argument is never empty
HTMLIZE_LINK_PREFIX := $(shell echo "-$$($(QUERY_CFG_CMD) html_prefix)")
$(HTMLIZE_OUT) $(COPY_OUT): $(HTMLIZE_IN) $(COPY_IN) $(DEPS)
	@for el in $(HTMLIZE_OUT_FILE) $(CURRENT_DIR)/$(HTMLIZE_FILE) \
		$(HTMLIZE_LINK_PREFIX) \
		$(IN_DIR) $(OUT_DIR) $(HTMLIZE_IN); \
		do echo $$el; done > $(HTMLIZE_TMP_FILE)
	@$(HTMLIZE_SCRIPT) 2>/dev/null
	@find . $(EXCL_FILE_PATTERN) -exec rm '{}' ';'

# create submodules and dependent packages
$(SUBMODULE_PROOFS):
	@git submodule update --init --recursive

$(BROWSERIFY_BUNDLE): $(patsubst %,$(NODE_DIR)/%,$(BROWSERIFY_CONVERSIONS)) \
		$(BROWSERIFY_CC)
	@echo -n 'npm modules [ '
	@for module in $(BROWSERIFY_CONVERSIONS); do \
		echo -n "$$module "; done
	@echo "] => $@"
	@$(BROWSERIFY_CC) -r $(BROWSERIFY_CONVERSIONS) | \
		$(UGLIFY_JS) $(UGLIFY_JS_OPTS) > $@
$(NODE_DEPS):
	@npm install

$(HTML_PARSER):
	@cpan HTML::TokeParser::Simple

$(ORG_LOADDEFS):
	@$(MAKE) -C $(ORG_MODE_DIR)

sweep:
	@find $(OUT_DIR) $(EXCL_FILE_PATTERN) -exec rm '{}' ';'
	@find $(IN_DIR) $(EXCL_FILE_PATTERN) -exec rm '{}' ';'
	@rm -f $(HTMLIZE_TMP_FILE) $(HTMLIZE_OUT_FILE)

clean: sweep
	@rm -f $(HTMLIZE_OUT) $(OUT_PAGES) $(COPY_OUT)
	@rm -rf $(ORG_INFO_OUT_DIR) $(OUT_STYLES_DIR) $(OUT_SCRIPTS_DIR)
	@$(MAKE) -C $(ORG_INFO_DIR) clean
	@rm -f $(HTMLIZE_TMP_FILE) $(IN_DIR)/sitemap.{org,html,org.html}
	@rm -f $(BROWSERIFY_BUNDLE)

distclean: clean
	@rm -rf $(NODE_DIR)
	@git submodule deinit -f $(CURRENT_DIR)

rebuild:
	$(MAKE) clean
	$(MAKE) all
