# Environment variables.

watch:
	@find . -name "*.markdown" -or -name "*.html" | entr sh -c "site build"

.PHONY: watch
