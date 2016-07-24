help:
	@echo "Usage: make COMMAND"
	@echo
	@echo "Commands:"
	@echo "  help             display this message"
	@echo "  dep-install      install dependencies"
	@echo "  test             run tests"
	@echo "  update-headers   update library headers"

dep-install:
	cask install

test:
	cask exec buttercup -L . -L tests

update-headers:
	cask exec org-doc -c utf-8 README.org stardict.el

.PHONY: help dep-install test update-headers
