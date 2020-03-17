all: setup build test lint

GITHUB_RELEASE ?= $(HOME)/.local/bin/github-release

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool hlint weeder

.PHONY: setup.release
setup.release:
	curl -L https://github.com/tfausak/github-release/releases/download/1.2.5/github-release-linux.gz | gunzip > github-release
	chmod +x github-release
	mv -v github-release $(GITHUB_RELEASE)

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --fast --pedantic --test

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint .
	stack exec $(STACK_ARGUMENTS) weeder .

RELEASE_TAG ?= v$(shell sed '/^version: \(.*\)$/!d; s//\1/' package.yaml)
RELEASE_OS_NAME ?= linux

.PHONY: release.build
release.build:
	bin/release-build ./star-cache
	tar -czf star-cache.tar.gz star-cache
	$(RM) -f star-cache

.PHONY: release.tag
release.tag:
	git tag --sign --message $(RELEASE_TAG) $(RELEASE_TAG)
	git push --follow-tags

.PHONY: release.github
release.github:
	$(GITHUB_RELEASE) release \
	  --token=$$GITHUB_TOKEN \
	  --owner=freckle \
	  --repo=star-cache \
	  --title=$(RELEASE_TAG) \
	  --tag=$(RELEASE_TAG)
	$(GITHUB_RELEASE) upload \
	  --token=$$GITHUB_TOKEN \
	  --owner=freckle \
	  --repo=star-cache \
	  --file=star-cache.tar.gz \
	  --tag=$(RELEASE_TAG) \
	  --name=star-cache-$(RELEASE_TAG)-$(RELEASE_OS_NAME)-x86_64.tar.gz

.PHONY: release
release: release.build release.tag release.github

.PHONY: release.master
release.master: RELEASE_TAG=master
release.master: release.build release.github

.PHONY: clean
clean:
	stack clean
