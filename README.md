# CodinGame Elm Template

This template is designed for the puzzle ["Skynet Revolution"][skynet]
but can easily be adapted to other CodinGame puzzles and contests.

```sh
# Install dev dependencies
npm install

# Compile, minify (terser), beautify (prettier), concatenate, copy to clipboard
npx node build.js
```

> Note: copy to clipboard is not supported on Linux system using Wayland.
> In such cases just copy yourself the content of `build/code.js`

[skynet]: https://www.codingame.com/training/medium/skynet-revolution-episode-1

## How does it work? (JS side)

It's quite easy actually.
The trick is simply to give up the priority on the event loop
once per game loop iteration with `setTimeout()`.

Here is the skeleton of `CodinGame.js`:

```js
// Copy the initialization readline calls from the JS codingame starter template.
initData = {};
...

// Init Elm app with initial data.
const app = this.Elm.Main.init({ flags: initData });

// Transfer Elm command to STDOUT.
app.ports.command.subscribe(answer => console.log(answer));

// Transfer Elm debug to STDERR.
app.ports.debug.subscribe(msg => console.error(msg));

// Game loop.
(function gameLoop() {
  // Send turn data to Elm for processing.
  app.ports.incoming.send(readLinesIntoTurnData());

  // Give up priority on the event loop to enable
  // subscription to elm outgoing port to trigger.
  setTimeout(gameLoop);
})();

// Update turnData with the new turn data.
// Performs side effects (readline)
function readLinesIntoTurnData() {
  ...
}
```

## How does it work? (Elm side)

It is a normal [`Platform.worker`][worker] program with 3 ports set up.

```elm
-- Retrieve the updated game data every turn.
port incoming : (Value -> msg) -> Sub msg

-- Port to give the new command for this turn.
port command : String -> Cmd msg

-- Port to help debugging, will print using console.error().
port debug : String -> Cmd msg
```

In the update function, after computing our strategy,
we generate a string and send it to the `command` port
so that CodinGame executes our command.

[worker]: https://package.elm-lang.org/packages/elm/core/latest/Platform#worker

## Adapt to another puzzle or contest

1. In `CodinGame.js`, change `initData` initialization code by copying stuff from codingame JS starter template.
2. In `Main.elm`, change the `InitData` type and associated decoder.
3. In `CodinGame.js`, change the content of `readLinesIntoTurnData()` function by copying stuff from codingame JS starter template.
4. In `Main.elm`, change the `TurnData` type and associated decoder.
5. In `Main.elm`, change the game logic for each turn.
