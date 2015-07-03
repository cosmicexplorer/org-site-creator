.PHONY: all clean serve

JEKYLL_CONFIG_FILE := _config.yml
ORG_DIR := org
OUT_DIR := $(shell grep "source:" $(JEKYLL_CONFIG_FILE) | sed -e 's/^source://g')
ORG_IN := $(shell find $(ORG_DIR) -name "*.org")
OUT_PAGES := $(patsubst $(ORG_DIR)/%.org,$(OUT_DIR)/%.html,$(ORG_IN))
JEKYLL_OUT := _site

all: $(OUT_PAGES)

html/%.html: org/%.org | $(OUT_DIR)
	./migrate_org.el $(ORG_DIR) $(OUT_DIR) $<

$(OUT_DIR):
	mkdir $@

clean:
	@rm -rf $(OUT_DIR)
	@rm -rf $(JEKYLL_OUT)

serve: all
# insert file-watching logic here
	@jekyll serve
