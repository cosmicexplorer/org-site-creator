.PHONY: all clean rebuild serve

JEKYLL_CONFIG := _config.yml
ORG_DIR := org
OUT_DIR := $(shell ./parse_config.sh source $(JEKYLL_CONFIG))
ORG_IN := $(shell find $(ORG_DIR) -name "*.org")
OUT_PAGES := $(patsubst $(ORG_DIR)/%.org,$(OUT_DIR)/%.html,$(ORG_IN))
JEKYLL_OUT := $(shell ./parse_config.sh destination $(JEKYLL_CONFIG))
JEKYLL_SERVE_PORT := $(shell ./parse_config.sh port $(JEKYLL_CONFIG))

all: $(OUT_PAGES)

html/%.html: org/%.org
	./migrate_org.el $(ORG_DIR) $(OUT_DIR) $<

clean:
	@rm -rf $(OUT_DIR)
	@rm -rf $(JEKYLL_OUT)

rebuild:
	$(MAKE) clean
	$(MAKE) all

serve: rebuild
	@./watch_recompile.coffee $(JEKYLL_SERVE_PORT) \
		./migrate_org.el $(ORG_DIR) $(OUT_DIR)
