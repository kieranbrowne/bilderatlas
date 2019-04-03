all: build open

open:
	open public/index.html

build:
	elm make Pack.elm --output=public/index.js

deploy: build
	git subtree push --prefix resources/public/ origin gh-pages

