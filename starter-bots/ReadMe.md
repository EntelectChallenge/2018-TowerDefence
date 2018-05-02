# Starter Bots

Entelect will provide Starter Bots for C#, Python, Javascript and Java. Starter bots are basic started projects that can read game files and make random moves. Reference bots that are capable of playing a game from start to finish are also included for contestants wishing to have something more to work from.

For any additional languages we will be relying on the community contributing a starter bot for the language of their choice. If you would like your language to be supported you will have to submit a starter bot. Starter bot submissions will close at Midnight on the 27th of May, after this no additional starter bots will be accepted.

Calibration bots will be included into the game engine before the first battle (after starter bot submission have closed) and will be based on the starter bot for each language.

## Starter Bot Submissions

Follow these steps to submit a starter bot.

1. Clone this repository.
1. Create the starter bot in the language of your choice.
1. Include a readme of any environment configuration and setup guide for your language on the tournament server.
1. Create a new pull request for your starter bot.
1. After peer review we will consider your starter bot for inclusion in the tournament.
1. If the pull request is merged then the starter bot and language can be considered officially included and supported for the tournament.

### Starter Bot Submission Rules

Please ensure your starter bot follow these rules:

1. Has a `bot.json` file.
1. Can compile on any system and is in running order (Should not produce any errors when executing).
1. Reads in the `state.json` file and parses that to a structure supported in your language.
   * The configurations for buildings including cost, health, construction time, damage given, energy generated, map width, map height etc. must be read from the state file. It SHOULD NOT be hardcoded in your starter bot. 
1. If there is a row that is under attack, and you have enough energy for a wall, build a wall at a random unoccupied location on that row.
   * A row is under attack if: there is an enemy Attack unit on the row, and NO ally Defence buildings on that row.
1. Else if you have enough energy for the most expensive building, build a random building type at a random unoccupied location.
1. If you can't do one of the above two, save energy until you have enough for the most expensive building.
