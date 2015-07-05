.PHONY: all clean distclean rebuild serve
.DELETE_ON_ERROR:

THIS_MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(patsubst %/,%,$(dir $(THIS_MAKEFILE_PATH)))

# npm
NODE_DIR := node_modules
NPM_BIN = $(shell npm bin)
COFFEE_CC = $(NPM_BIN)/coffee

# submodules
HTMLIZE_DIR := htmlize
HTMLIZE_FILE := $(HTMLIZE_DIR)/htmlize.el
HTMLIZE_PROOF := $(HTMLIZE_DIR)README.md
ORG_INFO_DIR := org-info-js
ORG_INFO_FILE := $(ORG_INFO_DIR)/org-info.js
ORG_STYLE_FILE := $(ORG_INFO_DIR)/stylesheet.css
ORG_INFO_PROOF := $(ORG_INFO_DIR)/README.md
MY_COLOR_THEME_DIR := $(CURRENT_DIR)/emacs-color-themes
MY_COLOR_THEME_FILE := $(MY_COLOR_THEME_DIR)/color-theme-danny.el

SUBMODULES := $(HTMLIZE_DIR) $(ORG_INFO_DIR) $(MY_COLOR_THEME_DIR)
SUBMODULE_PROOFS := $(HTMLIZE_PROOF) $(ORG_INFO_PROOF)

# build scripts
SETUP_DIR := setup
DEPS := $(HTMLIZE_FILE) $(NODE_DIR) $(wildcard $(SETUP_DIR)/*)

# we read from this to setup everything else
JEKYLL_CONFIG := _config.yml

SWITCH_DIR_SCRIPT := $(SETUP_DIR)/switch-dir.sh

ORG_PATTERN := -type f -name "*.org" -not -iwholename "*$(ORG_INFO_DIR)/*"
ORG_DIR := $(CURRENT_DIR)
ORG_IN := $(shell find $(ORG_DIR) $(ORG_PATTERN) | sort | uniq)
OUT_DIR := cosmicexplorer.github.io
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
COLOR_THEME_DIR := $(CURRENT_DIR)/color-theme-6.6.0
PROJ_FILE_PATTERN := -type f \
	$(patsubst %,-not -iwholename "*%/*", $(GIT_DIR) $(NODE_DIR) \
		$(OUT_DIR) $(ORG_INFO_DIR) $(MY_COLOR_THEME_DIR) \
		$(HTMLIZE_DIR) $(COLOR_THEME_DIR))

HTMLIZE_PATTERN := -not -name "*.html" $(PROJ_FILE_PATTERN)
HTMLIZE_IN := $(shell find $(CURRENT_DIR) $(HTMLIZE_PATTERN))
HTMLIZE_OUT := $(patsubst %,%.html, \
	$(shell $(SWITCH_DIR_SCRIPT) $(CURRENT_DIR) $(OUT_DIR) \
		$(HTMLIZE_IN)))

COPY_PATTERN := -name "*.html" $(PROJ_FILE_PATTERN)
COPY_IN := $(shell find $(CURRENT_DIR) $(COPY_PATTERN))
COPY_OUT := $(shell $(SWITCH_DIR_SCRIPT) $(CURRENT_DIR) $(OUT_DIR) $(COPY_IN))

ORG_INFO_OUT_DIR := $(OUT_DIR)/$(ORG_INFO_DIR)
ORG_INFO_OUT := $(ORG_INFO_OUT_DIR)/stylesheet-mini.css \
	$(ORG_INFO_OUT_DIR)/org-info-mini.js

OUT_DIRS := $(OUT_SCRIPTS_DIR) $(OUT_STYLES_DIR) $(ORG_INFO_OUT_DIR)

all: $(OUT_PAGES) $(OUT_SCRIPTS) $(OUT_STYLES) $(HTMLIZE_OUT) \
	$(HTMLIZE_MAKEFILE) $(COPY_OUT) $(ORG_INFO_OUT) | $(OUT_DIRS)

$(OUT_DIRS):
	mkdir -p $@

# scripts
$(OUT_SCRIPTS_DIR)/%.js: $(SCRIPTS_DIR)/%.coffee
	@echo "$< => $@"
	@$(COFFEE_CC) -bc --no-header -o $(OUT_SCRIPTS_DIR) $<
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
	@echo "[" $(ORG_INFO_DIR)/*-mini* "] => ($(ORG_INFO_OUT_DIR))"
	@cp $(ORG_INFO_DIR)/*-mini* $(ORG_INFO_OUT_DIR)

# make html from org
MIGRATE_SCRIPT := $(SETUP_DIR)/migrate_org.el
$(OUT_PAGES): $(ORG_IN) $(DEPS)
	@$(MIGRATE_SCRIPT) $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $(ORG_IN) \
		1>&2 2>/dev/null

# htmlize
HTMLIZE_TMP_FILE := tmpfile
HTMLIZE_SCRIPT := $(SETUP_DIR)/htmlize_file.sh \
	$(shell cat $(CURRENT_DIR)/xvfb.config) $(HTMLIZE_TMP_FILE)
COLOR_THEME_FILE := $(COLOR_THEME_DIR)/color-theme.el
$(HTMLIZE_OUT): $(HTMLIZE_IN)
	@for el in $(HTMLIZE_FILE) $(COLOR_THEME_FILE) \
		$(MY_COLOR_THEME_FILE) $(ORG_DIR) $(OUT_DIR) $(HTMLIZE_IN); \
		do echo $$el >> $(HTMLIZE_TMP_FILE); done
	@$(HTMLIZE_SCRIPT) 2>/dev/null

# create submodules and dependent packages
$(SUBMODULE_PROOFS):
	@git submodule update --init --recursive
$(NODE_DIR):
	@npm install

clean: $(DEPS)
	@$(MAKE) -C $(ORG_INFO_DIR) clean
	@$(MAKE) -C $(OUT_DIR) clean
	@find . -type f -name "*~" -exec rm '{}' ';'

distclean: clean
	@rm -rf $(NODE_DIR)
	@git submodule deinit $(CURRENT_DIR)

rebuild:
	$(MAKE) clean
	$(MAKE) all
