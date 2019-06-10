PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

.DEFAULT_GOAL := build

node_modules:
	npm i

build: node_modules
	@elm make src/Main.elm --output=public/main.js
	@cp src/index.html public

ci-build: node_modules
	@elm make src/Main.elm --optimize --output=public/main.js
	@cp src/index.html public
