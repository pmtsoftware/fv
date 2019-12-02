default: build

build:
	elm make src/Main.elm --output=main.js

clean:
	rm -f *.js
	rm -rf ./elm-stuff/
