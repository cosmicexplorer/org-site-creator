.PHONY: all clean rebuild serve

SETUP_DIR := setup

HTMLIZE_DIR := htmlize
HTMLIZE_FILE := $(HTMLIZE_DIR)/htmlize.el

DEPS := $(HTMLIZE_FILE)

ORG_PATTERN := "*.org"
RESOURCES_REGEX := ".+\\.(js|css)"

JEKYLL_CONFIG := _config.yml
ORG_DIR := org
ORG_IN := $(shell find $(ORG_DIR) -type f -name $(ORG_PATTERN))
RESOURCES_IN := $(shell find $(ORG_DIR) -type f -regex $(RESOURCES_REGEX))

SCRIPTS_DIR := scripts
STYLES_DIR := styles

OUT_DIR := $(shell $(SETUP_DIR)/parse_config.sh source $(JEKYLL_CONFIG))
OUT_PAGES := $(patsubst $(ORG_DIR)/%.org,$(OUT_DIR)/%.html,$(ORG_IN))
OUT_RESOURCES := $(patsubst $(ORG_DIR)/%,$(OUT_DIR)/%,$(RESOURCES_IN))

JEKYLL_OUT := $(shell $(SETUP_DIR)/parse_config.sh destination $(JEKYLL_CONFIG))
JEKYLL_SERVE_PORT := $(shell $(SETUP_DIR)/parse_config.sh port $(JEKYLL_CONFIG))

all: $(OUT_PAGES) $(OUT_RESOURCES)

$(OUT_DIR)/$(SCRIPTS_DIR)/%: $(ORG_DIR)/$(SCRIPTS_DIR)/% $(DEPS)
	@$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $<

$(OUT_DIR)/$(STYLES_DIR)/%: $(ORG_DIR)/$(STYLES_DIR)/% $(DEPS)
	@$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $<

$(OUT_DIR)/%.html: $(ORG_DIR)/%.org $(DEPS)
	@$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) $(OUT_DIR) $<

$(DEPS):
	git submodule update --init --recursive

clean:
	@find . -not -iwholename "*$(ORG_DIR)*" -regex ".+\\.(html|css|js)" \
		-exec rm '{}' ';'
	@find . -regex ".+\\.?#.*" -exec rm '{}' ';'
	@rm -rf $(JEKYLL_OUT)

rebuild:
	$(MAKE) clean
	$(MAKE) all

serve: rebuild
	@$(SETUP_DIR)/watch_recompile.coffee $(JEKYLL_SERVE_PORT) \
		$(SETUP_DIR)/migrate_org.el $(HTMLIZE_FILE) $(ORG_DIR) \
		$(OUT_DIR)
