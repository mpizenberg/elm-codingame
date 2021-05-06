const fs = require("fs");
const spawn = require("cross-spawn");
const clipboardy = require("clipboardy");

// Use cross-spawn to correctly handle npm binaries in Windows.
function exec(command, args, options = {}) {
  options.stdio = options.stdio || ["pipe", "pipe", "inherit"];
  const result = spawn.sync(command, args, options);
  if (result.error != null) {
    console.error(result.error);
    process.exit(1);
  }
  if (result.status != 0) {
    process.exit(result.status || 1);
  }
  return result.stdout;
}

// Create the build/ directory that will contain all build artifacts.
fs.mkdirSync("build", { recursive: true });

// Compile the Main.elm file.
exec("elm", ["make", "Main.elm", "--optimize", "--output", "build/Elm.js"], {
  stdio: "inherit",
});

// Minify the generated elm code with dead code elimination to avoid reaching CodinGame file limit.
const minified = exec("terser", [
  "build/Elm.js",
  `--compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe'`,
]);
exec("terser", ["--mangle", "--output", "build/Elm.terser.js"], {
  input: minified,
});

// Use prettier because otherwise the single line is too long for CodinGame.
const prettified = exec("prettier", ["build/Elm.terser.js"]);
fs.writeFileSync("build/Elm.prettier.js", prettified);

// Generate the final complete file by concatenation.
const header = `
// Original code in Elm, available at:
// https://github.com/mpizenberg/elm-codingame
// The following contains the JavaScript code
// result of the compilation of the Elm code.
`;
const codingameJS = fs.readFileSync("CodinGame.js");
const finalCode = `${header}${prettified}${codingameJS}`;
fs.writeFileSync("build/code.js", finalCode);

// Copy final code to clipboard.
clipboardy.writeSync(finalCode);
