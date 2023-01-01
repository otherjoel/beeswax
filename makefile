SHELL = /bin/bash

scribble: scribblings/beeswax.scrbl
scribble: ## Rebuild Scribble docs
	rm -rf scribblings/beeswax/* || true
	cd scribblings && scribble --htmls +m --redirect https://docs.racket-lang.org/local-redirect/ beeswax.scrbl

publish: ## Sync Scribble HTML docs to web server (doesn’t rebuild anything)
	rsync -av --delete scribblings/beeswax/ $(JDCOM_SRV)what-about/beeswax/

# Self-documenting makefile (http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html)
help: ## Displays this help screen
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

.PHONY: help publish scribble

.DEFAULT_GOAL := help
