# Game Rules for 2018 - Tower Defence
## Game description:
* The aim of the game is to find the best strategy to place your buildings to defend your base and defeat the opposing player.
* The game is in the style of a 1v1 match where each player gives a command per turn.
* Your objective is to get projectiles through the opposing player's defences and damage their main base directly. Once a player base's health drops to 0, then that player has lost.

## The Map:
* A map will be **{X}** by **{X}** in size.
* The map is mirrored, and each player will look at the map from the left-hand side.
* The player can only build buildings in their half of the map.
* The coordinates for a cell on the map takes the form of **'X,Y'** starting from 0, e.g. the coordinates **'0,0'** will be the top left cell.
* The entire map will be visible to both players, including the opposing player's side of the map.

## Commands:
**Note: A player can only send one command at a time. Any extra commands will be ignored.**

### The commands are:
* **Build** a building of any type, if there is enough energy for that building.
* **Do nothing** to conserve energy.
* Any invalid command will result in a **'Do nothing'** command.
* Invalid commands include:
	* Trying to place a building when you don’t have enough energy.
	* Trying to place a building on coordinates that is currently occupied by another building.
	* Trying to place a building on your opponent’s side of the board or out of bounds.

### Command Format
* The command format is '**x,y,building_type**'.
* The input must be comma-separated numbers that represent the coordinates followed by the numeric id of the building type.
* The valid building types are:
	* 0: **Wall (Defence)**
	* 1: **Turret (Attack)**
	* 2: **Energy Generator (Utility)**
* An example of a valid command will be '**0,0,1**', which will place a turret at the top left of the map.

## Economy:
* Each player will receive {X} energy at the start of their turn.
* Generators can be built to increase the amount received per turn, more information can be found in the [building types](#building-types) section.
* Energy is needed to be able to build any building.
* A player will receive energy every turn of the game - The amount of energy received is **{X}** per turn, without any generators. Generators will be discussed in the [building types](#building-types) section.
* Energy is needed to be able to build a building of any type.



## Buildings:
* Each building will have an energy cost to build.
* Each building will take turns until it is fully built and ready, based on the types.
* A building that is not fully built will be destroyed in one hit.
* The energy cost of a building is removed once the command to build a building has been successful.
* Trying to place a building without enough energy for that building will result in a **'Do nothing'** command.
* Once a building is destroyed, another building can be placed at that location without penalty.
* Only one building of any type can occupy a location on the map.
* No buildings of the opposing player may be added to the players own side.

### Building types:
**The player can build one of 3 different types of buildings**

#### Wall (Defence):
A defensive building, that is tougher than any other building type with no offensive capabilities.
Able to take more hits than any other building, these cheap buildings are great for soaking up damage while your offensive buildings fire from safety behind them.

**Building details:**
* Cost: {x}
* Health: {x}
* Build time {x}
* Completed Icon: {x}
* Not Completed Icon: {x}

#### Turret (Attack):
The main offensive building you can build that fires straight line projectiles toward the opposing side. This will be the main way to damage the opposing player’s buildings and base.
The Turret has a firing rate, meaning it will fire a projectile every {x} turns after it has been successfully built.

**Building details:**
* Cost:	{x}
* Health: {x}
* Firing rate: {x}
* Damage: {x}
* Build time {x}
* Completed Icon: {x}
* Not Completed Icon: {x}

#### Generator (Utility):
A utility building that provides **{x}** extra energy per turn. These buildings have lower health and can take hits before it is destroyed.

**Building details:**
* Cost:	{x}
* Health: {x}
* Energy per turn: {x}
* Build time {x}
* Completed Icon: {x}
* Not Completed Icon: {x}

## Projectiles
* Currently only turrets will create projectiles.
* A projectile will always deal all its damage on the building it hits and stop, even if the building hit has less health than the damage dealt.
* There is no limit to how many projectiles can be in a single cell at one time.
* Projectiles do not interact with each other, meaning opposing projectiles will not stop or impact one another.
* Projectiles will hit the first opposing building or continue on to hit the opposing player’s base.
* Projectiles currently move at minimum one cell per turn, depending on the projectile speed, until it impacts either an opposing player or the opposing player's base.
* Projectiles currently only move in a straight line toward the opposing player.
* Projectiles move at least one cell the moment it is created.

## Score
Each player will have a score based on the value of the buildings that the player destroyed of his opponent.
**Note that a building that is not fully built will reward the full amount of points.**

The scores for each building types are:
* Turret (Attack): {x}
* Wall (Defence): {x}
* Generator (Utility): {x}

**Scores will be used to determine a victor in the event of a stalemate or draw.**

## Game Engine Rules
* Each player gets a turn, and the turns run at the same time for both players.
* Each bot will get a maximum time of 5 seconds to execute each command.
* There will be a maximum of 1000 turns for each side, at which point the game the victor will be determined by the highest score.
* The game engine will process commands in the following order:
	* Building will be created, based on the commands from the player.
	* Projectiles will be generated from any attack buildings, if they can fire that turn.
	* The projectiles will be immediately moved, based on their speed.
	* Each projectile will hit a building, if it hit it during the movement phase.
	* Dead buildings will be removed.
	* Scores will be given to each player, depending on destroyed buildings.
	* Resources will be added, based on the baseline amount received and the number of energy buildings a player has.
