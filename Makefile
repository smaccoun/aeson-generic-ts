all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: test
test:
	stack test

.PHONY: lint
lint:
	hlint .
	weeder .

