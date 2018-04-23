# Game Rules for 2018 - Tower Defence
## Game description:
•	The game is a face off - And every player can give a command per turn.
•	To win, a player must get projectiles trough the defending players board
to damage the other player directly. Once a player has no more health, they have lost.

The Map:
•	A map will be {X} by {X} in size. The map is mirrored, and each player will look at the map from the left-hand side.
•	The player can only place buildings in their halve of the map.
•	The address for a cell in the map takes the form of X,Y - starting at a 0.
•	This means that 0,0 is the address of the top left cell.
•	The complete map will always be visible for both players, and there is no fog of war.

Commands:
A player can only send one command at a time. Note that extra commands will be ignored.
	The commands are: 
	Place a building of any type, if there is enough energy for that building.
	Do nothing to conserve energy.
	Any invalid command will result in a 'Do nothing' command.
	Invalid commands include:
•	Trying to place a building when you don’t have enough energy.
•	Trying to place a building on coordinates that is currently occupied by another building.
•	Trying to place a building on your opponent’s side of the board or out of bounds.
	Command Format
	The command format is 'x,y,building_type'.
	The input must be comma-separated numbers that represent the coordinates followed by the numeric id of the building type. 
	The valid building types are:
•	0: Wall (Defensive)
•	1: Turret (Offensive)
•	2: Energy Generator (Utility)
	An example of a valid command will be '0,0,1', which will place a turret at the top left of the map.
Economy:
	Each player will receive {X} energy at the start of their turn.
	Generators can be built to increase the amount received per turn, more information can be found in the Buildings section.
	Energy is needed to be able to build any building.
		
	A player will receive energy every turn of the game - The amount of energy received is X per turn, without any generators. Generators will be discussed in the building types section.
	Energy is needed to be able to build a building of any type.



Buildings:
	The game is about the best way to place your buildings:
	Each building will have an energy cost to build.
	Each building will take turns until it is fully built and ready, based on the types.
	A building that is not fully built will be destroyed in one hit.
	The energy cost of a building is removed once the command to build a building has been successful.
	Trying to place a building without enough energy for that building will result in a 'Do nothing' command.
	Once a building is destroyed, another building can be placed at that location without penalty.
	Only one building of any type can occupy a location on the map.
	No buildings of the opposing player may be added to the players own side.
	
Building types:
The player can build one of 3 different types of buildings:
Wall (Defence):
A defensive building, that is tougher than any other building type with no offensive capabilities.
Able to take more hits than any other building, these cheap buildings are great for soaking up damage while your offensive buildings fire from safety behind them.
	Stats:
o	Cost:	{x}
o	Health: {x}
o	Build time {x}
o	Completed Icon: {x}
o	Not Completed Icon: {x}

Turret (Attack):		
The main offensive building you can build that fires straight line projectiles toward the opposing side. This will be the main way to damage the opposing player’s buildings and base.
The Turret has a firing rate, meaning it will fire a projectile every {x} turns after it has been successfully built.
Stats:
	Cost:	{x}
	Health: {x}
	Build time {x}
	Completed Icon: {x}
	Not Completed Icon: {x}
	Firing rate: {x}
	projectile strength: {x}

Energy Building:		
A utility building that provides {x} extra energy per turn. These buildings have lower health and can take hits before it is destroyed. 
Stats:
	Cost:	{x}
	Health: {x}
	Build time {x}
	Completed Icon: {x}
	Not Completed Icon: {x
	Energy per turn: {x}
		
Projectiles
	Currently only turrets will create projectiles.
	A projectile will always deal all its damage on the building it hits and stop, even if the building hit has less health than the damage dealt.
	There is no limit on how many projectiles can be in a cell.
	Projectiles do not interact with each other, meaning opposing projectiles will not stop or impact one another.
	Projectiles will hit the first opposing building or continue on to hit the opposing player’s base.
	A projectile will always move one cell toward the enemy per turn, until it hits a building or damages a player directly.
	A projectile can only move in the lane that it is created in.
	A projectile will move one cell the moment it is created.  		
		
Score	
A player will have a score - The score is based on the player that won, as well as points for every building of the enemy destroyed by the player.
Note that a building that is not fully built will reward the full amount of points.
The scores for buildings destroyed are:
	Defence Building: {x} – Change to defence
	Attack Building: {x} – Change to attack
	Energy Building: {x} – Change to energy
	
To Add:	
Game Engine Rules
•	Each player gets a turn, and the turns is at the same time for both players.
•	Each bot will get a time of 5 seconds to execute its commands.
•	There will be a maximum of 1000 turns for each side, at which point the game will be given to the player with the highest score.
•	The game engine will process commands in the following order:
o	Building will be created, based on the commands from the player.
o	Projectiles will be generated from any attack buildings, if they can fire that turn.
o	The projectiles will be immediately moved, based on their speed.
o	Each projectile will hit a building, if it hit it during the movement phase.
o	Dead buildings will be removed.
o	Scores will be given to each player, depending on destroyed buildings.
o	Resources will be added, based on the baseline amount received and the number of energy buildings a player has.

		
