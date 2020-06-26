SHELL := /bin/bash
ROOT := .

all: elm

default: all

elm:
	elm make --debug src/Main.elm --output main.js

elm-production:
	elm make --optimize src/Main.elm --output main.js

production: elm-production

zip: elm-production
	zip giraffe.zip index.html elm-pep.js main.js
