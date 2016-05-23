all:
	rm -rf ./dist
	mkdir ./dist
	cp ./index.html ./dist/index.html
	cp ./main.css ./dist/main.css
	pulp browserify --optimise --to ./dist/app.js
