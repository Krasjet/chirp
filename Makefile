.PHONY: test build ghci ghci-test ghcid doc

test:
	@stack test

build:
	@stack build

ghci:
	@stack ghci

ghci-test:
	@stack ghci --test

ghcid:
	@ghcid \
		--command "stack ghci --test" \
		--test "main"

doc:
	@stack haddock --open libkst
