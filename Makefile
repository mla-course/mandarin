.DEFAULT_GOAL := help

SHELL := /bin/sh

CLJ       ?= clojure
CLJ_REPL  ?= clj
KONDO     := $(CLJ) -M:lint
SRC_DIRS  := src test

.PHONY: help test test-var lint lint-strict \
        repl cider clean check deps run

help: ## Show available tasks
	@awk 'BEGIN {FS = ":.*## "} \
		/^[a-zA-Z_-]+:.*## / { \
			printf "  %-16s %s\n", $$1, $$2 \
		}' $(MAKEFILE_LIST)

deps: ## Resolve dependencies (including the test, lint and cider aliases)
	$(CLJ) -P -M:test:lint:cider

test: ## Run the test suite
	$(CLJ) -M:test

test-var: ## Run one test: make test-var T=pinyin-of
	@test -n "$(T)" || { echo "usage: make test-var T=<deftest-name>"; exit 2; }
	$(CLJ) -M:test-paths -e \
	  "(require 'clojure.test 'mla.mandarin-test) \
	   (clojure.test/test-vars [#'mla.mandarin-test/$(T)])"

lint: ## Lint source and tests with clj-kondo
	$(KONDO) --lint $(SRC_DIRS)

lint-strict: ## Lint with warnings treated as errors
	$(KONDO) --lint $(SRC_DIRS) --fail-level warning

check: lint test ## Run linting and tests

repl: ## Start a standard Clojure REPL
	$(CLJ_REPL) -M:repl

cider: ## Start an nREPL server with CIDER middleware
	$(CLJ) -M:cider

s ?= 你好！

run: ## Analyze a Mandarin sentence: make run [s=<text>]
	@escaped=$$(printf '%s' "$s" | sed 's/\\/\\\\/g; s/"/\\"/g'); \
	$(CLJ) -J-Dfile.encoding=UTF-8 -M -e \
	  "(require '[mla.mandarin :as m] '[clojure.pprint :refer [pprint]]) \
	   (pprint (m/analyze \"$${escaped}\"))"

clean: ## Remove generated build artifacts
	rm -rf target .cpcache .clj-kondo/.cache
