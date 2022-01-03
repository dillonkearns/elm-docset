all: build clean

build:
	npm run build

clean:
	find ./dist -name index.html -print | xargs -P 16 -I {} sh -c "echo {}&&node clean-html.js {}"
	find ./dist -name content.json -print | xargs rm -rf -v "removing: {}"
	rm ./dist/index.js ./dist/elm.js ./dist/elm-pages.js ./dist/all-paths.json 

