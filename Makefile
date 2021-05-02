build:
	mkdir -p build
	elm make Main.elm --optimize --output build/Elm.js

terser:
	terser build/Elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle > build/Elm.terser.js

prettier:
	prettier build/Elm.terser.js > build/Elm.prettier.js

all: build terser prettier
	echo "// Original code in Elm, available at:" > build/build.js
	echo "// https://github.com/mpizenberg/elm-codingame" >> build/build.js
	echo "// The following line is the JavaScript code" >> build/build.js
	echo "// Result of compilation of the elm code" >> build/build.js
	cat build/Elm.prettier.js >> build/build.js
	cat CodinGame.js >> build/build.js
	cat build/build.js | xsel -ib
