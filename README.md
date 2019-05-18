# CodingGame Elm Template

This template is designed for the contest ["A Code of Ice and Fire"][contest]
but can easily be adapted to other CodinGame puzzles and contests.

```bash
# Compile Elm code into Main.js
elm make src/Main.elm --output Main.js --optimize

# Concatenate Main.js and CodingGame.js
cat CodingGame.js >> Main.js

# Copy to clipboard
xclip -selection clipboard Main.js

# Paste into your CodingGame web editor and hit Play! ;)
```

Put all this into a makefile to _make_ your life easier.

[contest]: https://www.codingame.com/ide/challenge/a-code-of-ice-and-fire

## How does it work? (JS side)

It's quite easy actually.
The trick is simply to give up the priority on the event loop
once per game loop iteration with `setTimeout()`.

Here is the skeleton of `CodingGame.js`:

```js
// Retrieve initial data from input.
initData = {};
...

// Init Elm app with initial data.
const app = this.Elm.Main.init({ flags: initData });

// Setup subscription to elm outgoing port
// used to transfer the string to print.
app.ports.order.subscribe(answer => console.log(answer));

// We can also setup an error port for debug.
app.ports.debug.subscribe(msg => console.error(msg));

// Global gameData to minimize GC.
gameData = {};

// Game loop.
(function gameLoop() {
  // Read this turn game data.
  readLinesIntoGameData();

  // Send game turn data to elm for processing.
  app.ports.incomming.send(gameData);

  // Give up priority on the event loop to enable
  // subscription to elm outgoing port to trigger.
  setTimeout(gameLoop);
})();

// Update gameData with the new turn data.
// Performs side effects (readline)
function readLinesIntoGameData() {
	...
}
```

## How does it work? (Elm side)

It is a normal [`Platform.worker`][worker] program,
with 3 ports set up:

```elm
-- Port bringing the updated game data every turn.
-- This triggers the update function.
port incomming : (Value -> msg) -> Sub msg

-- Port to give the new orders for this turn.
port order : String -> Cmd msg

-- Port to help debugging, will print using console.error().
port debug : String -> Cmd msg
```

In the update function, after computing our strategy,
we generate a string and send it to the `order` port
so that CodingGame executes our command.

[worker]: https://package.elm-lang.org/packages/elm/core/latest/Platform#worker
