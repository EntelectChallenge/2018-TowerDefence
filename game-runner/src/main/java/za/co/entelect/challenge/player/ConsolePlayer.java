package za.co.entelect.challenge.player;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;

import java.util.Scanner;

public class ConsolePlayer extends Player {

    private static final Logger log = LogManager.getLogger(ConsolePlayer.class);

    private GameMapRenderer gameMapRenderer;
    private Scanner scanner;

    public ConsolePlayer(String name) {
        super(name);

        scanner = new Scanner(System.in);
        gameMapRenderer = new TowerDefenseConsoleMapRenderer();
    }

    @Override
    public RawCommand getPlayerCommand(GameMap gameMap) {

        String output = gameMapRenderer.render(gameMap, getGamePlayer());
        log.info(output);

        String inputPrompt = gameMapRenderer.commandPrompt(getGamePlayer());
        log.info(inputPrompt);

        String consoleInput = scanner.nextLine();

        return new RawCommand(consoleInput);
    }

    @Override
    public void gameEnded(GameMap gameMap) {

    }

    @Override
    public void playerKilled(GameMap gameMap) {
        log.info(String.format("Player %s has been killed", getName()));
    }

    @Override
    public void playerCommandFailed(GameMap gameMap, String reason) {
        log.info(String.format("Could not process player command: %s", reason));
    }

    @Override
    public void firstRoundFailed(GameMap gameMap, String reason) {
        log.info(reason);
        log.info("The first round has failed.");
        log.info("The round will now restart and both players will have to try again");
        log.info("Press any key to continue");

        scanner.nextLine();
    }
}
