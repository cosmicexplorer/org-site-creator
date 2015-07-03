.PHONY: all clean distclean rebuild serve

NODE_DIR := node_modules
NPM_BIN = $(shell npm bin)
COFFEE_CC = $(NPM_BIN)/coffee

SETUP_DIR := setup

HTMLIZE_DIR := htmlize
HTMLIZE_FILE := $(HTMLIZE_DIR)/htmlize.el

MIGRATE_SCRIPT := $(SETUP_DIR)/migrate_org.el

THIS_MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))

DEPS := $(HTMLIZE_FILE) $(NODE_DIR) $(wildcard $(SETUP_DIR)/*) \
	$(THIS_MKFILE_PATH)

ORG_PATTERN := "*.org"
RESOURCES_REGEX := ".+\\.(js|css)"

JEKYLL_CONFIG := _config.yml
ORG_DIR := org
ORG_IN := $(shell find $(ORG_DIR) -type f -name $(ORG_PATTERN))
RESOURCES_IN := $(shell find $(ORG_DIR) -type f -regextype posix-extended \
	-regex $(RESOURCES_REGEX))

SCRIPTS_DIR := scripts
STYLES_DIR := styles

OUT_DIR := $(shell $(SETUP_DIR)/parse_config.sh source $(JEKYLL_CONFIG))
OUT_PAGES := $(patsubst $(ORG_DIR)/%.org,$(OUT_DIR)/%.html,$(ORG_IN))
OUT_RESOURCES := $(patsubst $(ORG_DIR)/%,$(OUT_DIR)/%,$(RESOURCES_IN))

JEKYLL_OUT := $(shell $(SETUP_DIR)/parse_config.sh destination $(JEKYLL_CONFIG))
JEKYLL_SERVE_PORT := $(shell $(SETUP_DIR)/parse_config.sh port $(JEKYLL_CONFIG))

all: $(OUT_PAGES) $(OUT_RESOURCES)

$(OUT_DIR)/$(SCRIPTS_DIR)/%.js: $(ORG_DIR)/$(SCRIPTS_DIR)/%.js $(DEPS)
	@$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $<

$(OUT_DIR)/$(STYLES_DIR)/%.css: $(ORG_DIR)/$(STYLES_DIR)/%.css $(DEPS)
	@$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $<

$(OUT_DIR)/%.html: $(ORG_DIR)/%.org $(DEPS)
	@$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $<

$(HTMLIZE_FILE):
	git submodule update --init --recursive

$(NODE_DIR):
	@npm install

clean:
	@find . -name "*.html" -exec rm '{}' ';'
	@rm -rf $(SCRIPTS_DIR) $(STYLES_DIR) $(JEKYLL_OUT)
	@find . -regex ".+\\.?#.*" -exec rm '{}' ';'

distclean: clean
	@rm -rf $(NODE_DIR)

rebuild:
	$(MAKE) clean
	$(MAKE) all

serve: rebuild
	@$(COFFEE_CC) $(SETUP_DIR)/watch_recompile.coffee \
		$(JEKYLL_SERVE_PORT) . \
		$(ORG_DIR) $(SETUP_DIR) $(THIS_MKFILE_PATH)
