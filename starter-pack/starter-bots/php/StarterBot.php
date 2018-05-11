<?php
require("include/Bot.php");
require("include/GameState.php");

$outputFilename = "command.txt";
$inputFilename = "state.json";

//Initiate Game state and a new Bot
$state = new GameState($inputFilename);
$bot = new Bot($state);

//Write Bot's action to output file
$outputFile = fopen($outputFilename, "w");
fwrite($outputFile, $bot->decideAction());

fclose($outputFile);
