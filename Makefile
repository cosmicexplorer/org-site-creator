.PHONY: all clean rebuild serve

SETUP_DIR := setup

HTMLIZE_DIR := htmlize
HTMLIZE_FILE := $(HTMLIZE_DIR)/htmlize.el

DEPS := $(HTMLIZE_FILE)

JEKYLL_CONFIG := _config.yml
ORG_DIR := org
OUT_DIR := $(shell $(SETUP_DIR)/parse_config.sh source $(JEKYLL_CONFIG))
JEKYLL_OUT := $(shell $(SETUP_DIR)/parse_config.sh destination $(JEKYLL_CONFIG))
JEKYLL_SERVE_PORT := $(shell $(SETUP_DIR)/parse_config.sh port $(JEKYLL_CONFIG))

all: $(DEPS)
	$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR)

$(DEPS):
	git submodule update --init --recursive
clean:
	@find $(OUT_DIR) -name "*.html" -type f -exec rm '{}' ';'
	@rm -rf $(JEKYLL_OUT)

rebuild:
	$(MAKE) clean
	$(MAKE) all

serve: rebuild
	@$(SETUP_DIR)/watch_recompile.coffee $(JEKYLL_SERVE_PORT) \
		$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) \
		$(OUT_DIR)
