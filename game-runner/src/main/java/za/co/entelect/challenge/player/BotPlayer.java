package za.co.entelect.challenge.player;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.botrunners.BotRunner;
import za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer;
import za.co.entelect.challenge.core.renderers.TowerDefenseJsonGameMapRenderer;
import za.co.entelect.challenge.core.renderers.TowerDefenseTextMapRenderer;
import za.co.entelect.challenge.engine.runner.GameEngineRunner;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;
import za.co.entelect.challenge.utils.FileUtils;

import java.io.*;
import java.util.Scanner;

public class BotPlayer extends Player {
    private static final String BOT_COMMAND = "command.txt";
    private static final String BOT_STATE = "state.json";
    private static final String TEXT_MAP = "textMap.txt";
    private GameMapRenderer jsonRenderer;
    private GameMapRenderer textRenderer;
    private GameMapRenderer consoleRenderer;
    private Scanner scanner;
    private BotRunner botRunner;
    private String saveStateLocation;

    private static final Logger log = LogManager.getLogger(BotPlayer.class);

    public BotPlayer(String name, BotRunner botRunner, String saveStateLocation) {
        super(name);

        scanner = new Scanner(System.in);
        jsonRenderer = new TowerDefenseJsonGameMapRenderer();
        textRenderer = new TowerDefenseTextMapRenderer();
        consoleRenderer = new TowerDefenseConsoleMapRenderer();

        this.botRunner = botRunner;
        this.saveStateLocation = saveStateLocation;
    }

    @Override
    public void startGame(GameMap gameMap) {
        newRoundStarted(gameMap);
    }

    @Override
    public void newRoundStarted(GameMap gameMap) {
        String playerSpecificJsonState = jsonRenderer.render(gameMap, getGamePlayer());
        String playerSpecificTextState = textRenderer.render(gameMap, getGamePlayer());
        String playerSpecificConsoleState = consoleRenderer.render(gameMap, getGamePlayer());
        String consoleOutput = "";
        try {
            consoleOutput = runBot(playerSpecificJsonState, playerSpecificTextState);
        } catch (IOException e) {
            e.printStackTrace();
        }

        //receive response from bot
        String botInput = "";
        File botCommandFile = new File(String.format("%s/%s", botRunner.getBotDirectory(), BOT_COMMAND));
        Scanner scanner = null;
        try {
            scanner = new Scanner(botCommandFile);
            if (scanner.hasNext()) {
                botInput = scanner.nextLine();
            }else{
                botInput = "No Command";
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            log.info(String.format("File %s not found", botRunner.getBotDirectory() + "/" + BOT_COMMAND));
        }
        try{
            writeRoundStateData(playerSpecificJsonState, playerSpecificTextState,
                    playerSpecificConsoleState, botInput, gameMap.getCurrentRound(),
                    consoleOutput);
        }catch (IOException e){
            e.printStackTrace();
        }

        RawCommand rawCommand = new RawCommand(botInput);
        publishCommand(rawCommand);
    }

    private void writeRoundStateData(String playerSpecificJsonState, String playerSpecificTextState,
                                     String playerSpecificConsoleState ,String command, int round,
                                     String botConsoleOutput) throws IOException {
        String mainDirectory = String.format("%s/%s", saveStateLocation, FileUtils.getRoundDirectory(round));
        File fMain = new File(mainDirectory);
        if (!fMain.exists()){
            fMain.mkdirs();
        }

        File f = new File(String.format("%s/%s", mainDirectory, getName()));
        if (!f.exists()){
            f.mkdirs();
        }

        File fConsole = new File(String.format("%s/%s/%s", mainDirectory, getName(),"Console") );
        if (!fConsole.exists()){
            fConsole.mkdirs();
        }

        FileUtils.writeToFile(String.format("%s/%s/%s",mainDirectory, getName(), "JsonMap.json"), playerSpecificJsonState);
        FileUtils.writeToFile(String.format("%s/%s/%s",mainDirectory, getName(), "TextMap.txt" ), playerSpecificTextState);
        FileUtils.writeToFile(String.format("%s/%s/%s",mainDirectory, getName(), "PlayerCommand.txt"), command);
        FileUtils.writeToFile(String.format("%s/%s/%s/%s",mainDirectory, getName(), "Console", "Console.txt"), playerSpecificConsoleState);
        FileUtils.writeToFile(String.format("%s/%s/%s/%s",mainDirectory, getName(), "Console", "BotOutput.txt"), botConsoleOutput);
    }

    private String runBot(String state, String textState) throws IOException {
        File existingCommandFile = new File(String.format("%s/%s",  botRunner.getBotDirectory(), BOT_COMMAND));

        if (existingCommandFile.exists()){
            existingCommandFile.delete();
        }

        FileUtils.writeToFile(String.format("%s/%s", botRunner.getBotDirectory(),  BOT_STATE), state);
        FileUtils.writeToFile(String.format("%s/%s", botRunner.getBotDirectory(), TEXT_MAP), textState);

        String botConsoleOutput = "";

        try {
            botConsoleOutput = botRunner.run();
        }catch (IOException e){
            log.info("Bot execution failed: " + e.getLocalizedMessage());
        }
        log.info("BotRunner Started.");
        return botConsoleOutput;
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
