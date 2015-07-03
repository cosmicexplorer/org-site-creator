.PHONY: all clean serve

ORG_DIR := org
OUT_DIR := html
ORG_IN := $(wildcard $(ORG_DIR)/*.org)
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
	@jekyll serve --source $(OUT_DIR)
