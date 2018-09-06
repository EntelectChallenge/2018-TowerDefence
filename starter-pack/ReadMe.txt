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

 ____         ___        ___  
|___ \       / _ \      |__ \ 
  __) |     | | | |        ) |
 |__ <      | | | |       / / 
 ___) |  _  | |_| |  _   / /_ 
|____/  (_)  \___/  (_) |____|
                                         

Welcome to the starter pack for the 2018 Entelect Challenge!
Here you will find all that you'll need to run your first bot and compete in this year's challenge.



==== STEP 1 ====

Let's start off by running your very first match:
(Because we are going to run java files, please make sure you have java installed [http://www.oracle.com/technetwork/pt/java/javase/downloads/jdk8-downloads-2133151.html])

We included some 'run' files for various operating systems to run a match.

    On Windows:
        Simply run (double-click) the "run.bat" file.

    On Linux:
        Open the terminal in the starter pack's root directory and run the "make" or "make run" command.

You should now see some text whizz by in the console/terminal. If this is not the case, your match didn't run as it should. If the error message isn't clear, you can ask for help on our forum [forum.entelect.co.za].



==== STEP 2 ====

Now let's change things up a little bit. The previous match we ran, was between the Java reference bot and the Java starter bot. Let's change the match to be between the Java reference bot and a starter bot of your choice. To change this we need to edit the "config.json" file.

The format of the 'config.json' is as follows:

    "round-state-output-location" => This is the path to where you want the match folder in which each round's folder with its respective logs will be saved.

    "game-config-file-location" => This is the path to the game-config.properties file that is used to set various game-engine settings such as map size and building stats.

    "verbose-mode" => This is a true or false value to either print logs to the console or not respectively.

    "max-runtime-ms" => This is the amount of milliseconds that the game runner will allow a bot to run before making its command each round.

    "player-a" &
    "player-b" => This is the path to the folder containing the 'bot.json' file. If you would like to replace one of the bot players with a console player, just use the word "console" as the path.

The format of the 'bot.json' is as follows (also see the example in "/examples/example-bot.json"):

    "author" => This is the name of the person who wrote the bot.

    "email" => This is an email address where the author of the bot can be contacted if there are any questions.

    "nickName" => This is a nickname for the bot that will be used in visualisations.

    "botLocation" => This is a relative path to the folder containing the compiled bot file for the specific language.

    "botFileName" => This is the compiled bot file's name.

    "botLanguage" => This is the language name in which the bot is coded. A list of the currently supported languages and the names used in the runner can be found below.

To change the bots that play in the match, replace the "player-a" value in the "config.json" file with the path to another starter bots' folder containing "bot.json".

---- LANGUAGES ----

Java            => "java"
C# (.net core)  => "c#core"
Python3         => "python3"
Javascript      => "javascript"
Rust            => "rust"
C++             => "c++"
Kotlin          => "kotlin"
Golang          => "golang"
Haskell         => "haskell"
PHP             => "php"
LISP            => "lisp"
Scala           => "scala"



==== STEP 3 ====

All that is left to do is to modify the existing logic or to code your own, into one of the starter bots. This will require you to do some coding in the language of your choice.

Here is a brief explanation of how a bot should work:

    The bot will be called to run once per round in the game. After every round, the bot will be terminated and restarted for the next round.

    For each round the bot should go through the following process:

        Firstly read in the "state.json" file that contains the game map and all the round properties.

        Apply your logic to these properties and decide on what your next move is and where you want to apply it.

        Finally, write your move into the "command.txt" file.

Here are short explanations of the files mentioned above:

    "state.json" => This file keeps track of everything that is on the map, the player's health, energy and scores. For an example of a state file, see "/examples/example-state.json".

    "command.txt" => This file will be read by the game engine to know what your bot decided to do for that specific round. For an example of a command file, see "/examples/example-command.txt".



==== Prerequisites ====

Java bots
    The Java starter bot requires Java 8 or above.
    [http://www.oracle.com/technetwork/pt/java/javase/downloads/jdk8-downloads-2133151.html]

C# bots
    The C# core starter bot requires .NET Core runtime package 2.x.
    [https://www.microsoft.com/net/download/all]

Python2 bots
    The Python starter bot requires Python 2.7.
    [https://www.python.org/downloads/]

Python3 bots
    The Python starter bot requires Python 3.6.
    [https://www.python.org/downloads/]

Javascript bots
    The Javascript starter bot requires Node.Js 8.x.x or above.
    [https://nodejs.org/en/download/]

Rust bots
    For more info on the Rust bot, see the readme file or contact the person who submitted the bot, JWorthe (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/rust]

C++ bots
    For more info on the C++ bot, see the readme file or contact the person who submitted the bot, linjoehan (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/cplusplus]

Kotlin bots
    For more info on the Kotlin bot, see the source files or contact the person who submitted the bot, gkmauer (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/kotlin]

Golang bots
    For more info on the Golang bot, see the readme file or contact the person who submitted the bot, dougcrawford (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/golang]

Haskell bots
    For more info on the Haskell bot, see the readme file or contact the person who submitted the bot, Quiescent (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/haskell]

PHP bots
    For more info on the PHP bot, see the readme file or contact the person who submitted the bot, PuffyZA (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/php]

LISP bots
    For more info on the LISP bot, see the readme file or contact the person who submitted the bot, HenryS1 (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/common-lisp]

Scala bots
    For more info on the Scala bot, see the readme file or contact the person who submitted the bot, niekz (on GitHub)
    [https://github.com/EntelectChallenge/2018-TowerDefence/tree/master/starter-bots/scala]