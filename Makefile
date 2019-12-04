default: build

build:
	echo "Building.."
	elm make src/Main.elm --output=main.js

clean:
	rm -f *.js
	rm -rf ./elm-stuff/

run: build
	chromium-browser --incognito ./index.html
