 ______ _   _ _______ ______ _      ______ _____ _______ 
|  ____| \ | |__   __|  ____| |    |  ____/ ____|__   __|
| |__  |  \| |  | |  | |__  | |    | |__ | |       | |   
|  __| | . ` |  | |  |  __| | |    |  __|| |       | |   
| |____| |\  |  | |  | |____| |____| |___| |____   | |   
|______|_| \_|  |_|  |______|______|______\_____|  |_|   

  _____ _    _          _      _      ______ _   _  _____ ______ 
 / ____| |  | |   /\   | |    | |    |  ____| \ | |/ ____|  ____|
| |    | |__| |  /  \  | |    | |    | |__  |  \| | |  __| |__   
| |    |  __  | / /\ \ | |    | |    |  __| | . ` | | |_ |  __|  
| |____| |  | |/ ____ \| |____| |____| |____| |\  | |__| | |____ 
 \_____|_|  |_/_/    \_\______|______|______|_| \_|\_____|______|


Welcome to the starter pack for the 2018 Entelect Challenge!
Here you will find all that you'll need to run your first bot and compete in this year's challenge.



==== STEP 1 ====

Let's start off by running your very first match:
(Because we are going to run java files, please make sure you have java installed [http://www.oracle.com/technetwork/pt/java/javase/downloads/jdk8-downloads-2133151.html])

We included some 'run' files for various operating systems to run a match.

	On Windows:
		Simply run (double click) the "run.bat" file.

	On Linux:
		Open the terminal in the starter pack's root directory and run the "make" or "make run" command.

You should now see some text whizz by in the console/terminal. If this is not the case, your match didn't run as it should. If the error message isn't clear, you can ask for help on our forum [forum.entelect.co.za].



==== STEP 2 ====

Now let's change things up a little bit. The previous match we ran, was between the Java reference bot and the Java starter bot. Let's change the match to be between the Java reference bot and a starter bot of your choice. To change this we need to edit the "config.json" file.

The format of the of the "config.json" is as follows:

	"round-state-output-location" => This is the path to where you want the match folder in which each round's folder with its respective logs will be saved.

	"console-players" => This is the amount of console players that will be featured in the match. There are two options here, 0 when two bots should play against each other or 1 if you want to manually play against a bot.

	"max-runtime-ms" => This is the amount of milliseconds that the game runner will allow a bot to run before making its command each round.

	"bot-meta" => This is an array containing configurations for the bots that need to play in the match. The format for these are as follows:

		"botLocation" => This is a relative path to the folder containing the compiled bot file for the specific language.

		"botFileName" => This is the compiled bot file's name.

		"botLanguage" => This is the language name in which the bot is coded. The currently supported language names are 'java', 'javascript', 'python' and 'c#dotnetcore'.

To change the bots that play in the match, replace the first bot configuration with one of the example configurations below.


==== EXAMPLES ====

Here are the configuration examples for the various starter bots included in this starter pack:
(Note that for each of these, you need to make sure you have the specified language installed. See the prerequisites at the end of this file for more details.)

	Java
	{
		"botLocation": "./starter-bots/java/target",
		"botFileName": "java-sample-bot-1.0-SNAPSHOT-jar-with-dependencies.jar",
		"botLanguage": "java"
	}

	Javascript
	{
		"botLocation": "./starter-bots/javascript",
		"botFileName": "StarterBot.js",
		"botLanguage": "javascript"
	}

	Python
	{
		"botLocation": "./starter-bots/python",
		"botFileName": "StarterBot.py",
		"botLanguage": "python"
	}

	C#
	{
		"botLocation": "./starter-bots/csharpcore/StarterBot/bin/Debug/netcoreapp2.0",
		"botFileName": "StarterBot.dll",
		"botLanguage": "c#dotnetcore"
	}



==== STEP 3 ====

All that is left to do is to modify the existing logic, or to code your own, into one of the starter bots. This will require you to do some coding in the language of your choice.

Here is a brief explanation of how a bot should work:

	The bot wil be be called to run once per round in the game. After every round the bot will be terminated and restarted for the next round.

	For each round the bot should go through the following process:

		Firstly read in the "state.json" file that contains the game map and all the round properties.

		Apply your logic to these properties and decide on what your next move is and where you want to apply it.

		Finally write your move into the "command.txt" file.

Here is short explanations of the files mentioned above:

	"state.json" => This file keeps track of everything that is on the map, the players health, energy and scores. For an example of a state file, see "/examples/example-state.json".

	"command.txt" => This file will be read by the game engine to know what your bot decided to do for that specific round. For an example of a command file, see "/examples/example-command.txt".



==== Prerequisites ====

Java bots
	The Java starter bot requires Java 8 or above.
	[http://www.oracle.com/technetwork/pt/java/javase/downloads/jdk8-downloads-2133151.html]

C# bots
	The C# core starter bot requires .NET Core runtime package 2.x.
	[https://www.microsoft.com/net/download/all]

Python bots
	The Python starter bot requires Python 2.7.
	[https://www.python.org/downloads/]

Javascript bots
	The Javascript starter bot requires Node.Js 8.x.x or above.
	[https://nodejs.org/en/download/]
