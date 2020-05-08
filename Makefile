build:
	elm make src/Main.elm --output Main.js --optimize

all: build
	terser Main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle | terser --beautify --output=Main.min.js
	echo "// Original code in Elm, available at:" > build.js
	echo "// https://github.com/mpizenberg/elm-codingame/tree/spring-2020" >> build.js
	echo "// The following line is the JavaScript code" >> build.js
	echo "// Result of compilation of the elm code" >> build.js
	cat Main.min.js >> build.js
	cat CodinGame.js >> build.js
	xclip -selection clipboard build.js
