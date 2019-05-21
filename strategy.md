% Initial thoughts #############################################################

# Important rules:

- Territory must be connexe. Cut branches are deactivated and units are killed
- Territory brings income (1 per active cell)
- Units cost money (1 for level 1)
- Default payment of units -> clear units and income of player

Remarks:

The input "income" already is the balance of earnings and costs of current state.
The gold available at the beginning of next turn is exactly:
next gold = current gold at end of turn - income at beginning of next turn.
We just have to make sure when computing that our next income will be sufficient.

# Defensive plays:

- Reasonable treasory margins
- always have more income than expenses
- Keep territory connectivity factor high (minimum connectivity of two active cells)
- Defend HQ with units

# Gready plays: (long term growth)

- Low treasory margins
- Invest in units (keep balance near 0, or negative if treasory margins)
- No defense units

# Offensive plays:

- Cut enemy territory
- Target enemy HQ with units
- Target enemy units if we have a slight advantage

% Attack #######################################################################

# Cut enemy territory

If we have enough money, we can perform a training cut attack.
It consists in training units one after another on a path cutting
the opponent territory.
This requires at least 10 gold per cell on the path.

It is absolutely worth it if we can cut enough income such that
the opponent won't be able to pay its troops.
That case should be pretty rare though.

It is kind of worth it if we can destroy many troops + income.
To be balanced with the risk of loosing that path of troops at the opponent turn.

# Spear to opponent HQ

Similar to cutting territory but this time we go straight for the opponent HQ.
The deal is quite simple, if we have enough money for a straight line
of training units to reach the opponent HQ, it is victory!

% Economy ######################################################################

One tower is always worth it since it costs a level 3 units to destroy.

- tower: 15 gold
- level3: 30 gold + 20 / turn

Can be strategic to force opponent to build a level 3 unit,
and then build up an economic advantage.

% Preparation ##################################################################

At the first turn, we have more time to precompute a few things.
Those will be extremely useful during game loops that are way shorter.

# Distance to HQ

We should compute distance to our and opponent HQ for each valid cell.
This will enable things like estimating if we can spear to opponent HQ
or if we need to defend our HQ from the opponent doing the same.
We could even set traps if the opponent tries to reach for our HQ
in a risky way if it thinks it could almost do it.

Beware that there are things on the map that alter the "distance" to HQ.
Buildings like towers for example prevent units of level 1 and 2 to move.

# Path length from one HQ to the other

The path of shortest distance from our HQ to the opponent HQ.
This stored path can be "wide" i.e. we could store
the distance from optimal path.

Or just the length of the shortest path between to HQs passing by this cell.
This could enable detection of dead-ends.
Actually yeah this does not bring much info with 4-connectivity cells.
99% of the time, distance is just |x1-x2| + |y1-y2|.

# The "width" of each cell

How many cells are available from this cell at a distance of 1, 2, 3, 4, etc.
It is super important if a critical path also happens to be a bottleneck.
Control of a bottleneck in the critical path is high priority.
