all:
	elm make src/Main.elm --output Main.js --optimize
	cat CodinGame.js >> Main.js
	xclip -selection clipboard Main.js
