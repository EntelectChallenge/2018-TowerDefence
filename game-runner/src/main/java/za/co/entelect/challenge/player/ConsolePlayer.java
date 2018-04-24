package za.co.entelect.challenge.player;

import za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;

import java.util.Scanner;

public class ConsolePlayer extends Player {

    private GameMapRenderer gameMapRenderer;
    private Scanner scanner;

    public ConsolePlayer(String name) {
        super(name);

        scanner = new Scanner(System.in);
        gameMapRenderer = new TowerDefenseConsoleMapRenderer();
    }

    @Override
    public void startGame(GameMap gameMap) {
        newRoundStarted(gameMap);
    }

    @Override
    public void newRoundStarted(GameMap gameMap) {

        String output = gameMapRenderer.render(gameMap, getGamePlayer());
        System.out.println(output);

        String inputPrompt = gameMapRenderer.commandPrompt(getGamePlayer());
        System.out.println(inputPrompt);

        String consoleInput = scanner.nextLine();

        RawCommand rawCommand = new RawCommand(consoleInput);
        publishCommand(rawCommand);
    }

    @Override
    public void gameEnded(GameMap gameMap) {

    }

    @Override
    public void playerKilled(GameMap gameMap) {
        System.out.println(String.format("Player %s has been killed", getName()));
    }

    @Override
    public void playerCommandFailed(GameMap gameMap, String reason) {
        System.out.println(String.format("Could not process player command: %s", reason));
    }

    @Override
    public void firstRoundFailed(GameMap gameMap, String reason) {
        System.out.println(reason);
        System.out.println("The first round has failed.");
        System.out.println("The round will now restart and both players will have to try again");
        System.out.println("Press any key to continue");

        scanner.nextLine();
    }
}
