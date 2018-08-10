# Game Rules for 2018 - Tower Defence
## The Game:
* The aim of the game is to find the best strategy to place your buildings on a map to defend your base and defeat the opposing player.
* The game is in the style of a 1v1 match where each player provides a command per turn.
* Your objective is to get missiles through the opposing player's defences and damage their main base directly. The first player whose base's health drops to 0 or below loses the match.

## The Map:
* The map will be **16** by **8** in size.
* The map is mirrored, and each player will look at the map from the left-hand side.
* As a player, you will always be player A.
* The player can only build buildings in their half of the map.
* The coordinates for a cell on the map takes the form of **'X,Y'** starting from 0, e.g. the coordinates **'0,0'** will be the top left cell.
* The entire map, player information, and building information will be visible to both players, including the opposing player's buildings.

You will encounter 3 map files while running the game. The most common and self-explanatory map type is the Json map. Then you will also find a text file map and see a map in the console while the match is running.
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
* Each player will receive **20** energy at the start of their turn.
* Energy generators can be built to increase the amount of energy received per turn, more information can be found in the building types section.
* Energy is needed to be able to build any building.
* A player will receive energy every turn of the game - The amount of energy received is **5** per turn, without any energy generators.

## Commands:
**Note: A player can only make one command per turn. Any additional commands will be ignored.**

### Available Commands:
* **Build** a building of any type, if you have enough energy for that building.
* **Deconstruct** any one of your buildings, you will receive **5** energy for doing so.
* **Do nothing** to conserve energy.
* Any invalid command will result in a **'Do nothing'** command.
* Invalid commands include:
    * Trying to place a building when you don't have enough energy.
    * Trying to place a building on coordinates that are currently occupied by another building.
    * Trying to place a building on your opponent's side of the map or coordinates that are out of bounds.

### Command Format:
* The command format is '**x,y,building_type**'.
* The input must be comma-separated numbers that represent the coordinates followed by the numeric ID of the building type.
* The valid commands are:
    * 0: Build a **Defence Building**
    * 1: Build a **Attack Building**
    * 2: Build a **Energy Building**
	* 3: **Deconstruct** a building
	* 4: Build a **Tesla Tower**
	* 5: Activate the **Iron Curtain**
* An example of a valid command will be '**0,0,1**', which will place an attack building at the top left corner of the map.

## Buildings:
* Each building will have an energy cost to build.
* Each building will take a number of turns until it is fully built and ready, based on its type.
* Buildings that are busy being built will not be hit by missiles or lightning over it.
* The energy cost of a building is deducted from a player's energy total once the command to build a building has been successful.
* Trying to place a building without enough energy for that building will result in a **'Do nothing'** command.
* Once a building is destroyed, another building can be placed at that location.
* Only one building can occupy a location on the map.
* Damaging enemy buildings will award a score to you, depending on the building type.
* Constructing new buildings will award a score to you, depending on the building type.

### Building Types:
**The player can build one of 4 different types of buildings**

#### Defence Building:
A defensive building is tougher than any other building type with no offensive capabilities.
These buildings are great for soaking up damage while your offensive buildings fire safely from behind them

**Defence building details:**
* Cost: **30**
* Health: **20**
* Construction time: **3**
* Constructed character: **D**
* Under construction character: **d**

#### Attack Building:
An offensive building fires straight line missiles toward the opposing side. This is the primary method of damaging the opposing player's buildings and home base.
The attack building has a firing rate, it will fire a missile every **3** turns after it has been successfully built.

**Attack building details:**
* Cost:    **30**
* Health: **5**
* Firing rate: **3**
* Missile speed: **2**
* Damage: **5**
* Construction time: **1**
* Constructed character: **A**
* Under construction character: **a**

#### Energy Building:
A utility building that provides **3** extra energy per turn. These buildings have lower health and does not offer much in terms of defence.

**Energy building details:**
* Cost:    **20**
* Health: **5**
* Energy generated per turn: **3**
* Construction time: **1**
* Constructed character: **E**
* Under construction character: **e**

#### Tesla Tower:
A powerful but fragile attack building that can hit multiple enemy buildings in one hit. It costs **100** energy to fire a single shot, and this will
automatically hit all enemies in a pre-defined pattern, causing **20** damage per hit. Once it has cooled down, it will wait until it has enough energy
to fire, and only then reset the cooldown.

**Tesla tower details:**
* Cost:    **300**
* Health: **5**
* Damage: **20** per hit
* Construction time: **10**
* Constructed character: **T**
* Energy needed per attack:  **100**
* Under construction character: **t**
* Maximum Range: **9**
* Maximum amount of towers per person: **2**

#### Iron Curtain:
An impenetrable shield that protects your entire base from enemy Tesla Tower attacks, and will stop any enemy missiles hitting it...your own 
Tesla Tower attacks and missiles can pass through unobstructed. This shield costs **100** energy to activate, and is always placed on 
your most forward column, even if you have buildings on that column. The shield stays active for **6(Active rounds)** rounds, and can only 
be activated when available to the player. This availability will be reset on every game round that is a factor of **30(Reset period)**.

**Iron curtain details:**
* Cost:    **100**
* Active rounds: **6**
* Reset period: **30**

## Missiles
* Attack buildings will create missiles.
* A missile will deal all its damage to the building it hits, even if the building has less health than the damage dealt.
* A missile will disappear once it hits a building.
* There is no limit to how many missiles can coexist in one cell at a time.
* Missiles do not interact with each other.
* Missiles will try to continue on to hit the opposing player's base if there are no opponent buildings in the way.
* Missiles do not interact with your own buildings and simply pass over them.
* Missiles move at the missile speed as defined by the weapon that fired them.
* Missiles move in a straight line toward the opposing player.
* Missiles move the moment they are created.

## Lighting Attack
* Only the tesla tower can generate a lighting attack.
* The lighting attack is instant - An Attack building destroyed by a lighting attack will not fire a missile that turn.
* The lighting attack never damages your own buildings, only those of the enemy.
* The lighting attack will attack enemy buildings in 3 rows, with the middle row being the one the telsa tower is on.
* The lighting attack will always attack the highest(to the top of the map) enemy building for a column.
* It will try to strike an enemy building in every column until the maximum range of the tower, starting at the fist column next to the tower. 
* A lighting attack will not effect any friendly or enemy missiles in flight.

## Score
Each player will have a score based on the damage dealt by the player on both the opponent's health and buildings, as
well as the players own energy generated, and buildings built.

Scores are calculated by adding the following together:
* Total damage dealt to opponent buildings
* Fixed score for each building constructed
* Total energy generated
* Damage done to your opponent's health

**Scores will be used to determine a victor in the event of a stalemate or draw. If both bots draw in a tournament, a random winner will be selected, as only stagnant bots should result in this situation**

## Game Engine Rules
* Each player gets a turn, and the turns run simultaneously for both players. Each player's commands are captured, and only then the round is run.
* Players may store state between turns via text files in their bot folder.
* Each bot will get a maximum time of 2 seconds to execute each command.
* There will be a maximum of **400** turns for each side, at which point the victor of the game will be determined by the highest score.
* The game engine will process commands in the following order:
    * Building will be created, based on the commands from the player.
	* Tesla towers will attempt to fire, if enough energy is available.
    * Missiles will be generated from any attack buildings if they can fire that turn.
    * The missiles will be immediately moved, based on their speed.
    * Each missile will damage a building if it struck it during the movement phase.
    * Destroyed buildings will be removed.
    * Scores will be awarded to each player, depending on the round.
    * Energy will be awarded, based on the baseline amount received and the number of energy buildings a player has.
