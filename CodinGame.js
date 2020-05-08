// Retrieve initial data from input.
const initData = {};
var inputs = readline().split(" ");
initData.width = parseInt(inputs[0]); // size of the grid
initData.height = parseInt(inputs[1]); // top left corner is (x=0, y=0)
initData.rows = [];
for (let i = 0; i < initData.height; i++) {
  initData.rows.push(readline()); // " " is floor, "#" is wall
}

// Init Elm app with initial data.
const app = this.Elm.Main.init({ flags: initData });

// Setup subscription to elm outgoing port
// used to transfer the string to print.
app.ports.order.subscribe(console.log);

// We can also setup an error port for debug.
app.ports.debug.subscribe(console.error);

// Global gameData to minimize GC.
const gameData = {};

// Game loop.
(function gameLoop(turn) {
  // Read this turn game data.
  readLinesIntoGameData(turn);

  // Send game turn data to elm for processing.
  app.ports.incoming.send(gameData);

  // Give up priority on the event loop to enable
  // subscription to elm outgoing port to trigger.
  setTimeout(gameLoop, 0, turn + 1);
})(0);

// Update gameData with the new turn data.
// Performs side effects (readline)
function readLinesIntoGameData(turn) {
  // Turn
  gameData.turn = turn;

  // Score
  var inputs = readline().split(" ");
  gameData.myScore = parseInt(inputs[0]);
  gameData.opponentScore = parseInt(inputs[1]);

  // All your pacs and enemy pacs in sight
  gameData.pacs = [];
  gameData.visiblePacCount = parseInt(readline());
  for (let i = 0; i < gameData.visiblePacCount; i++) {
    var inputs = readline().split(" ");
    gameData.pacs.push({
      pacId: parseInt(inputs[0]), // pac number (unique within a team)
      mine: inputs[1] !== "0", // true if this pac is yours
      x: parseInt(inputs[2]), // position in the grid
      y: parseInt(inputs[3]), // position in the grid
      typeId: inputs[4], // unused in wood leagues
      speedTurnsLeft: parseInt(inputs[5]), // unused in wood leagues
      abilityCooldown: parseInt(inputs[6]), // unused in wood leagues
    });
  }

  // All pellets in sight
  gameData.pellets = [];
  gameData.visiblePelletCount = parseInt(readline());
  for (let i = 0; i < gameData.visiblePelletCount; i++) {
    var inputs = readline().split(" ");
    gameData.pellets.push({
      x: parseInt(inputs[0]),
      y: parseInt(inputs[1]),
      value: parseInt(inputs[2]),
    });
  }
}
