// Concatenate above the compiled file Elm.js obtained from
// elm make Main.elm --optimize --output=Elm.js

// Retrieve initial data from input.
initData = {};
let inputs = readline().split(" ");
initData.nodeCount = parseInt(inputs[0]); // the total number of nodes in the level, including the gateways
initData.linkCount = parseInt(inputs[1]); // the number of links
initData.exitCount = parseInt(inputs[2]); // the number of exit gateways
initData.links = [];
for (let i = 0; i < initData.linkCount; i++) {
  let inputs = readline().split(" ");
  const n1 = parseInt(inputs[0]); // n1 and n2 defines a link between these nodes
  const n2 = parseInt(inputs[1]);
  initData.links.push([n1, n2]);
}
initData.exits = [];
for (let i = 0; i < initData.exitCount; i++) {
  initData.exits.push(parseInt(readline())); // the index of a gateway node
}

// Init Elm app with initial data.
const app = this.Elm.Main.init({ flags: initData });

// Setup subscription to elm outgoing port
// used to transfer the string to print.
app.ports.command.subscribe((cmd) => console.log(cmd));

// We can also setup an error port for debug.
app.ports.debug.subscribe((msg) => console.error(msg));

// Start the game loop.
gameLoop();

// Game loop.
function gameLoop() {
  // Send game turn data to elm for processing.
  app.ports.incoming.send(readLinesIntoTurnData());

  // Give up priority on the event loop to enable
  // subscription to elm outgoing port to trigger.
  setTimeout(gameLoop, 0);
}

// Update turnData with the new turn data.
// Performs side effects (readline)
function readLinesIntoTurnData() {
  // The index of the node on which the Skynet agent is positioned this turn.
  const skynetNode = parseInt(readline());
  return { skynetNode };
}
