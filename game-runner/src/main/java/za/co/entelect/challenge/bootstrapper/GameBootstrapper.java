package za.co.entelect.challenge.bootstrapper;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;
import za.co.entelect.challenge.botrunners.BotRunner;
import za.co.entelect.challenge.botrunners.BotRunnerFactory;
import za.co.entelect.challenge.core.engine.TowerDefenseGameEngine;
import za.co.entelect.challenge.core.engine.TowerDefenseGameMapGenerator;
import za.co.entelect.challenge.core.engine.TowerDefenseRoundProcessor;
import za.co.entelect.challenge.engine.runner.GameEngineRunner;
import za.co.entelect.challenge.entities.BotMetaData;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;
import za.co.entelect.challenge.player.BotPlayer;
import za.co.entelect.challenge.player.ConsolePlayer;
import za.co.entelect.challenge.utils.FileUtils;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class GameBootstrapper {
    private static final Logger log = LogManager.getLogger(GameBootstrapper.class);
    
    private GameEngineRunner gameEngineRunner;
    private static String gameName;

    public static void main(String[] args) {

        GameBootstrapper gameBootstrapper = new GameBootstrapper();

        try {
            Config config = gameBootstrapper.loadConfig();

            gameBootstrapper.prepareEngineRunner(config);
            gameBootstrapper.prepareHandlers();
            gameBootstrapper.prepareGame(config);

            gameBootstrapper.startGame();

        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    private Config loadConfig() throws Exception {
        try (FileReader fileReader = new FileReader("./config.json")) {
            Gson gson = new GsonBuilder().create();
            Config config = gson.fromJson(fileReader, Config.class);

            if (config == null)
                throw new Exception("Failed to load config");

            return config;
        }
    }

    private void prepareEngineRunner(Config config) {
        gameEngineRunner = new GameEngineRunner();

        gameEngineRunner.setGameEngine(new TowerDefenseGameEngine(config.gameConfigFileLocation));
        gameEngineRunner.setGameMapGenerator(new TowerDefenseGameMapGenerator());
        gameEngineRunner.setGameRoundProcessor(new TowerDefenseRoundProcessor());
    }

    private void prepareHandlers() {
        gameEngineRunner.firstPhaseHandler = getFirstPhaseHandler();
        gameEngineRunner.gameStartedHandler = getGameStartedHandler();
        gameEngineRunner.gameCompleteHandler = getGameCompleteHandler();
        gameEngineRunner.roundStartingHandler = getRoundStartingHandler();
        gameEngineRunner.roundCompleteHandler = getRoundCompleteHandler();
    }

    private void prepareGame(Config config) throws Exception {
        String timeStamp = new SimpleDateFormat("yyyy.MM.dd.HH.mm.ss").format(new Date());
        gameName = FileUtils.getAbsolutePath(config.roundStateOutputLocation) + "/" + timeStamp;

        List<Player> players = new ArrayList<>();

        parsePlayer(config.playerAConfig, players, "A", config.maximumBotRuntimeMilliSeconds);
        parsePlayer(config.playerBConfig, players, "B", config.maximumBotRuntimeMilliSeconds);

        gameEngineRunner.preparePlayers(players);
        gameEngineRunner.prepareGameMap();

        if (config.isVerbose) {
            Configurator.setRootLevel(Level.DEBUG);
        } else {
            Configurator.setRootLevel(Level.ERROR);
        }
    }

    private void parsePlayer(String playerConfig, List<Player> players, String playerNumber, int maximumBotRuntimeMilliSeconds) throws Exception {
        if (playerConfig.equals("console")) {
            players.add(new ConsolePlayer(String.format("Player %s", playerNumber)));
        } else {
            BotMetaData botConfig = getBotMetaData(playerConfig);
            BotRunner botRunner = BotRunnerFactory.createBotRunner(botConfig, maximumBotRuntimeMilliSeconds);
            BotPlayer player = new BotPlayer(String.format("%s - %s", playerNumber, botConfig.getNickName()), botRunner, gameName);
            players.add(player);
        }
    }

    private BotMetaData getBotMetaData(String botLocation) throws Exception {
        try (FileReader fileReader = new FileReader(String.format("%s/bot.json", botLocation))) {

            Gson gson = new GsonBuilder().create();

            BotMetaData botMeta = gson.fromJson(fileReader, BotMetaData.class);

            botMeta.setRelativeBotLocation(botLocation);

            if (botMeta == null)
                throw new Exception("Failed to load bot meta data from location: " + botLocation);

            return botMeta;
        }
    }

    private void startGame() throws Exception {
        gameEngineRunner.startNewGame();
    }

    private Consumer<GameMap> getFirstPhaseHandler() {
        return gameMap -> {
        };
    }

    private BiConsumer<GameMap, Integer> getRoundCompleteHandler() {
        return (gameMap, round) -> {
            log.info("=======================================");
            log.info("Round ended " + round);
            log.info("=======================================");
        };
    }

    private BiConsumer<GameMap, List<Player>> getGameCompleteHandler() {
        return (gameMap, players) -> {
            Player winner = null;
            StringBuilder winnerStringBuilder = new StringBuilder();
            for (Player player :
                    players) {
                if (player.getGamePlayer() == gameEngineRunner.getWinningPlayer()) {
                    winner = player;
                }
                winnerStringBuilder.append(player.getName() + "- score:" + player.getGamePlayer().getScore()
                        + " health:" + player.getGamePlayer().getHealth() + "\n");
            }

            if (winner == null) {
                log.info("=======================================");
                log.info("The game ended in a tie");
                log.info("=======================================");
            } else {
                log.info("=======================================");
                log.info("The winner is: " + winner.getName());
                log.info("=======================================");
            }

            BufferedWriter bufferedWriter = null;
            try {
                String roundLocation = String.format("%s/%s/endGameState.txt", gameName, FileUtils.getRoundDirectory(gameMap.getCurrentRound() - 1));
                bufferedWriter = new BufferedWriter(new FileWriter(new File(roundLocation)));

                winnerStringBuilder.insert(0, "The winner is: " + winner.getName() + "\n\n");

                bufferedWriter.write(winnerStringBuilder.toString());
                bufferedWriter.flush();
                bufferedWriter.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        };
    }

    private BiConsumer<GameMap, Integer> getRoundStartingHandler() {
        return (gameMap, round) -> {
            log.info("=======================================");
            log.info("Starting round " + round);
            log.info("=======================================");
        };
    }

    private Consumer<GameMap> getGameStartedHandler() {
        return gameMap -> {
            log.info("=======================================");
            log.info("Starting game");
            log.info("=======================================");
        };
    }
}
