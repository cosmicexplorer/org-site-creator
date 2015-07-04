.PHONY: all clean distclean rebuild serve
.DELETE_ON_ERROR:

THIS_MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

# npm
NODE_DIR := node_modules
NPM_BIN = $(shell npm bin)
COFFEE_CC = $(NPM_BIN)/coffee

# submodules
HTMLIZE_DIR := htmlize
HTMLIZE_FILE := $(HTMLIZE_DIR)/htmlize.el
ORG_INFO_DIR := org-info-js
ORG_INFO_FILE := $(ORG_INFO_DIR)/org-info.js

SUBMODULES := $(HTMLIZE_DIR) $(ORG_INFO_DIR)
SUBMODULE_PROOFS := $(HTMLIZE_FILE) $(ORG_INFO_FILE)

# build scripts
SETUP_DIR := setup
DEPS := $(HTMLIZE_FILE) $(NODE_DIR) $(wildcard $(SETUP_DIR)/*) \
	$(THIS_MAKEFILE_PATH)

# we read from this to setup everything else
JEKYLL_CONFIG := _config.yml

ORG_PATTERN := -type f -name "*.org"
ORG_DIR := .
ORG_IN := $(shell find $(ORG_DIR) $(ORG_PATTERN))
OUT_DIR := $(shell $(SETUP_DIR)/parse_config.sh source $(JEKYLL_CONFIG))
OUT_PAGES := $(patsubst $(ORG_DIR)/%.org,$(OUT_DIR)/%.html,$(ORG_IN))

ORG_INFO_MINI := $(patsubst %.js,%-mini.js,$(ORG_INFO_FILE))
ORG_INFO_STYLE := $(ORG_INFO_DIR)/stylesheet.css
ORG_INFO_MINI_OUT := $(patsubst $(ORG_INFO_DIR)/%,$(OUT_DIR)/%,$(ORG_INFO_MINI))
ORG_INFO_STYLE_OUT := $(patsubst $(ORG_INFO_DIR)/%,$(OUT_DIR)/%, \
	$(ORG_INFO_STYLE))
ORG_INFO_DEPS := $(ORG_INFO_MINI_OUT) $(ORG_INFO_STYLE_OUT)

# now htmlize every source file
GIT_DIR := .git
HTMLIZE_PATTERN := -type f -not -name "*.html" \
	-not -iwholename "*$(GIT_DIR)/*" -not -iwholename "*$(NODE_DIR)/*" \
	-not -iwholename "*$(OUT_DIR)/*"
HTMLIZE_IN := $(shell find $(CURRENT_DIR) $(HTMLIZE_PATTERN))
HTMLIZE_OUT := $(patsubst $(ORG_DIR)/%,$(OUT_DIR)/%.html, $(HTMLIZE_IN))

SCRIPTS_DIR := scripts
OUT_SCRIPTS := $(patsubst %.coffee,%.js, \
	$(shell find $(SCRIPTS_DIR) -name "*.coffee"))

all: $(OUT_PAGES) $(OUT_SCRIPTS) $(HTMLIZE_OUT)

%.js: %.coffee
	@echo "$< => $@"
	@$(COFFEE_CC) -bc --no-header $<

HTMLIZE_SCRIPT := $(SETUP_DIR)/htmlize_this_file.el
%.html: %
	@$(HTMLIZE_SCRIPT) $(HTMLIZE_FILE) $<

MIGRATE_SCRIPT := $(SETUP_DIR)/migrate_org.el
$(OUT_DIR)/%.html: $(ORG_DIR)/%.org $(DEPS) $(ORG_INFO_DEPS)
	@$(MIGRATE_SCRIPT) $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $<

$(ORG_INFO_MINI_OUT): $(ORG_INFO_MINI) $(SUBMODULE_PROOFS) $(DEPS)
	@echo "$< => $@"
	@cp $< $@

$(ORG_INFO_STYLE_OUT): $(ORG_INFO_STYLE) $(SUBMODULE_PROOFS) $(DEPS)
	@echo "$< => $@"
	@cp $< $@

$(ORG_INFO_MINI): $(SUBMODULE_PROOFS)
	$(MAKE) -C $(ORG_INFO_DIR)

$(SUBMODULE_PROOFS):
	@git submodule update --init --recursive

$(NODE_DIR):
	@npm install

JEKYLL_OUT := $(shell $(SETUP_DIR)/parse_config.sh destination $(JEKYLL_CONFIG))
clean:
	@rm -f $(OUT_PAGES) $(OUT_SCRIPTS) $(ORG_INFO_DEPS) $(HTMLIZE_OUT)
	@$(MAKE) -C $(ORG_INFO_DIR) clean
	@rm -rf $(JEKYLL_OUT)

distclean: clean
	@rm -rf $(NODE_DIR)
	@git submodule deinit .

rebuild:
	$(MAKE) clean
	$(MAKE) all

JEKYLL_SERVE_PORT := $(shell $(SETUP_DIR)/parse_config.sh port $(JEKYLL_CONFIG))
serve: rebuild
	@$(COFFEE_CC) $(SETUP_DIR)/watch_recompile.coffee \
		$(JEKYLL_SERVE_PORT) . \
		$(ORG_DIR) $(SETUP_DIR) $(THIS_MAKEFILE_PATH)
