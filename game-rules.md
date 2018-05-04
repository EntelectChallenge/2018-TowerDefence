# Game Rules for 2018 - Tower Defence
## The Game:
* The aim of the game is to find the best strategy to place your buildings on a map to defend your base and defeat the opposing player.
* The game is in the style of a 1v1 match where each player provides a command per turn.
* Your objective is to get missiles through the opposing player's defences and damage their main base directly. The first player whose base's health drops to 0 or below loses the match.

## The Map:
* The map will be **{X}** by **{Y}** in size.
* The map is mirrored, and each player will look at the map from the left-hand side.
* As a player, you will always be player A.
* The player can only build buildings in their half of the map.
* The coordinates for a cell on the map takes the form of **'X,Y'** starting from 0, e.g. the coordinates **'0,0'** will be the top left cell.
* The entire map, player information, and building information will be visible to both players, including the opposing player's units.

**{X} and {Y} will be variable.**

You will encounter 3 maps while running the game. The most common and self-explanatory map type is the Json map. Then you will also find a text file map and see a map in the console while the match is running.
Below we will explain some of the details for the text and console maps.

### Text File Map
The text file contains the following sections: game-info, player details, game map details, building details and missile details.

Each cell in the map is represented by [].

The format of the cell is [x,y,B,M1,M2] where the symbols mean the following:
* x: x coordinate
* y: y coordinate
* B: Building where A = attack, D = defense, E = energy and N = no building. 
* M1: Player 1's missiles, where the number shown is the number of missiles in that cell belonging to player 1.
* M2: Player 2's missiles, where the number shown is the number of missiles in that cell belonging to player 2.

For buildings, a lower-case letter means the building is under construction. An upper-case letter means the building is constructed.

The next section shows more information about the buildings on the map.
The format of the building details is as follows:
[x,y] Owner|ConstructionTimeLeft|Health|WeaponCooldownTimeLeft|WeaponDamage|EnergyGeneratedPerTurn 

The next section shows more information about the missiles on the map.
The format of the missile details is as follows:
[x,y] Owner|Damage 

### Console Map
Each cell in the console is represented by [].
In the console Player 1 is on the left and player 2 is on the right.

The format of the cell is [M2, x, B, y, M1] where the symbols mean the following:
* x: x coordinate
* y: y coordinate
* B: Building where A = attack, D = defense, E = energy and a blank space represents no building. 
* M1: Player 1's missiles, each missile is depicted by a > symbol which represents the direction it is travelling in.
* M2: Player 2's missiles, each missile is depicted by a < symbol which represents the direction it is travelling in.

The building will be a lower-case letter if it is still under construction and it will be an upper-case letter if it is already constructed.

## Economy:
* Each player will receive **{START_ENERGY}** energy at the start of their turn.
* Energy generators can be built to increase the amount of energy received per turn, more information can be found in the building types section.
* Energy is needed to be able to build any building.
* A player will receive energy every turn of the game - The amount of energy received is **{ENERGY_PER_TURN}** per turn, without any energy generators.

**{START_ENERGY} and {ENERGY_PER_TURN} will be variable.**

## Commands:
**Note: A player can only make one command per turn. Any additional commands will be ignored.**

### Available Commands:
* **Build** a building of any type, if you have enough energy for that building.
* **Do nothing** to conserve energy.
* Any invalid command will result in a **'Do nothing'** command.
* Invalid commands include:
    * Trying to place a building when you don't have enough energy.
    * Trying to place a building on coordinates that are currently occupied by another building.
    * Trying to place a building on your opponent's side of the map or coordinates that are out of bounds.

### Command Format:
* The command format is '**x,y,building_type**'.
* The input must be comma-separated numbers that represent the coordinates followed by the numeric ID of the building type.
* The valid building types are:
    * 0: **Defence Building**
    * 1: **Attack Building**
    * 2: **Energy Building**
* An example of a valid command will be '**0,0,1**', which will place an attack building at the top left corner of the map.

## Buildings:
* Each building will have an energy cost to build.
* Each building will take a number of turns until it is fully built and ready, based on its type.

* Buildings that are busy being built will not be hit by missles travelling over it.

* The energy cost of a building is deducted from a player's energy total once the command to build a building has been successful.
* Trying to place a building without enough energy for that building will result in a **'Do nothing'** command.
* Once a building is destroyed, another building can be placed at that location without penalty.
* Only one building of any type can occupy a location on the map.

### Building Types:
**The player can build one of 3 different types of buildings**

#### Defence Building:
A defensive building, that is tougher than any other building type with no offensive capabilities.
Able to take more hits than any other building, these buildings are great for soaking up damage while your offensive buildings fire from safety behind them.

**Defence building details:**
* Cost: **30**
* Health: **20**
* Construction time: **3**
* Constructed character: **D**
* Under construction character: **d**

#### Attack Building:
The main offensive building you can build that fires straight line missiles toward the opposing side. This will be the main way to damage the opposing player's buildings and base.
The attack building has a firing rate, meaning it will fire a missile every **{Fire rate}** turns after it has been successfully built. 

**Attack building details:**
* Cost:    **30**
* Health: **5**
* Firing rate: **3**
* Damage: **5**
* Construction time: **1**
* Constructed character: **A**
* Under construction character: **a**

#### Energy Building:
A utility building that provides **{Energy generated per turn}** extra energy per turn. These buildings have lower health and does not offer much in terms of defence.

**Energy building details:**
* Cost:    **20**
* Health: **5**
* Energy generated per turn: **3**
* Construction time: **1**
* Constructed character: **E**
* Under construction character: **e**

## Missiles
* Currently only attack buildings will create missiles.
* A missile will always deal all its damage on the building it hits firsts and stops, even if the building hit has less health than the damage dealt.
* There is no limit to how many missiles can be in a single cell at one time.
* Missiles do not interact with each other, meaning opposing missiles will not stop or impact one another.
* Missiles will hit any opposing building or continue on to hit the opposing player's base if there are no opponent buildings in the way.
* Missiles do not interact with your own buildings and simply pass over them.
* Missiles currently move at a minimum of one cell per turn, depending on the missile speed, until it impacts either an opposing player or the opposing player's base.
* Missiles currently only move in a straight line toward the opposing player.
* Missiles move at least one cell the moment it is created.

## Score
Each player will have a score based on the damage dealt by the player on both the opponent's health and buildings, as
well as the players own energy generated and buildings built.

Scores are calculated by adding the following together:
* Total damage dealt to opponent buildings
* Fixed score for each building constructed
* Total energy generated
* Damage bonus for any damage done to your opponent's health

Note that damage bonus is awarded each time your opponent's health pool is damaged, the player receives points equal to damage dealt times 100.

**Scores will be used to determine a victor in the event of a stalemate or draw. If both bots draw in a tournament, a random winner will be selected, as only stagnant bots should result in this situation**

## Game Engine Rules
* Each player gets a turn, and the turns run at the same time for both players. This means that each player's commands are captured, and only then is the round run.
* Players may store state between turns via files in their bot folder.
* Each bot will get a maximum time of 2 seconds to execute each command.
* There will be a maximum of **{MAXIMUM_TURNS}** turns for each side, at which point the victor of the game will be determined by the highest score.
* The game engine will process commands in the following order:
    * Building will be created, based on the commands from the player.
    * Missiles will be generated from any attack buildings if they can fire that turn.
    * The missiles will be immediately moved, based on their speed.
    * Each missile will hit a building if it hit it during the movement phase.
    * Destroyed buildings will be removed.
    * Scores will be awarded to each player, depending on the round.
    * Energy will be awarded, based on the baseline amount received and the number of energy buildings a player has.

**{MAXIMUM_TURNS} will be variable.**

