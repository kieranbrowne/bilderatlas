all: build open

open:
	open public/index.html

build:
	elm make Pack.elm --output=public/index.js

deploy:
	git subtree push --prefix public/ origin gh-pages

