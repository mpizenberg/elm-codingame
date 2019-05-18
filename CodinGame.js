// Retrieve initial data from input.
initData = {};
initData.numberMineSpots = parseInt(readline());
initData.mines = [];
for (let i = 0; i < initData.numberMineSpots; i++) {
  const inputs = readline().split(" ");
  const thisMine = {};
  thisMine.x = parseInt(inputs[0]);
  thisMine.y = parseInt(inputs[1]);
  initData.mines.push(thisMine);
}

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
  // Money.
  gameData.gold = parseInt(readline());
  gameData.income = parseInt(readline());
  gameData.opponentGold = parseInt(readline());
  gameData.opponentIncome = parseInt(readline());

  // Terrain, game map.
  gameData.terrain = [];
  for (let i = 0; i < 12; i++) {
    gameData.terrain.push(readline());
  }

  // Buildings.
  gameData.buildingCount = parseInt(readline());
  gameData.buildings = [];
  for (let i = 0; i < gameData.buildingCount; i++) {
    const inputs = readline().split(" ");
    const thisBuilding = {};
    thisBuilding.owner = parseInt(inputs[0]);
    thisBuilding.buildingType = parseInt(inputs[1]);
    thisBuilding.x = parseInt(inputs[2]);
    thisBuilding.y = parseInt(inputs[3]);
    gameData.buildings.push(thisBuilding);
  }

  // Units.
  gameData.unitCount = parseInt(readline());
  gameData.units = [];
  for (let i = 0; i < gameData.unitCount; i++) {
    const inputs = readline().split(" ");
    const thisUnit = {};
    thisUnit.owner = parseInt(inputs[0]);
    thisUnit.unitId = parseInt(inputs[1]);
    thisUnit.level = parseInt(inputs[2]);
    thisUnit.x = parseInt(inputs[3]);
    thisUnit.y = parseInt(inputs[4]);
    gameData.units.push(thisUnit);
  }
}
