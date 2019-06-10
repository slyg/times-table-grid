.DEFAULT_GOAL := build

build:
	@elm make src/Main.elm --output=public/main.js
	@cp src/index.html public
