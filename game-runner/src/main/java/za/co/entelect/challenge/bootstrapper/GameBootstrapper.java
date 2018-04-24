package za.co.entelect.challenge.bootstrapper;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import za.co.entelect.challenge.botrunners.BotRunner;
import za.co.entelect.challenge.botrunners.BotRunnerFactory;
import za.co.entelect.challenge.core.engine.TowerDefenseGameEngine;
import za.co.entelect.challenge.core.engine.TowerDefenseGameMapGenerator;
import za.co.entelect.challenge.core.engine.TowerDefenseRoundProcessor;
import za.co.entelect.challenge.engine.exceptions.InvalidRunnerState;
import za.co.entelect.challenge.engine.runner.GameEngineRunner;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;
import za.co.entelect.challenge.player.BotPlayer;
import za.co.entelect.challenge.player.ConsolePlayer;
import za.co.entelect.challenge.utils.FileUtils;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class GameBootstrapper {

    private GameEngineRunner gameEngineRunner;
    private static String gameName;

    public static void main(String[] args) {
        GameBootstrapper gameBootstrapper = new GameBootstrapper();

        try {
            Config config = gameBootstrapper.loadConfig();

            gameBootstrapper.prepareEngineRunner();
            gameBootstrapper.prepareHandlers();
            gameBootstrapper.prepareGame(config);

            gameBootstrapper.startGame();

        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    private Config loadConfig() throws Exception {

        FileReader fileReader = null;
        try {
            fileReader = new FileReader("./config.json");

            Gson gson = new GsonBuilder().create();


            Config config = gson.fromJson(fileReader, Config.class);
            if (config == null)
                throw new Exception("Failed to load config");

            return config;
        }

        finally {
            if (fileReader != null)
                fileReader.close();
        }
    }

    private void prepareEngineRunner() {
        gameEngineRunner = new GameEngineRunner();

        gameEngineRunner.setGameEngine(new TowerDefenseGameEngine());
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

    private void prepareGame(Config config) throws InvalidRunnerState {
        String timeStamp = new SimpleDateFormat("yyyy.MM.dd.HH.mm.ss").format(new Date());
        gameName = FileUtils.getAbsolutePath(config.roundStateOutputLocation) + "\\"+ timeStamp;
        //add console players
        List<Player> players = IntStream.range(0, config.consolePlayers)
                .mapToObj(i -> new ConsolePlayer(String.format("Player %d", i + 1)))
                .collect(Collectors.toList());

        //add bot players
        for (int i = 0; i < config.botMetaData.length; i++){
            BotRunner botRunner = BotRunnerFactory.createBotRunner(config.botMetaData[i], config.maximumBotRuntimeMiliSeconds);
            BotPlayer player = new BotPlayer(String.format("Player %d", config.consolePlayers + i + 1), botRunner, gameName);
            players.add(player);
        }

        gameEngineRunner.preparePlayers(players);
        gameEngineRunner.prepareGameMap();
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
            System.out.println("=======================================");
            System.out.println("Round ended " + round);
            System.out.println("=======================================");
        };
    }

    private BiConsumer<GameMap, List<Player>> getGameCompleteHandler() {
        return (gameMap, players) -> {
            Player winner = null;
            StringBuilder winnerStringBuilder = new StringBuilder();
            for (Player player :
                    players) {
                if (player.getGamePlayer() == gameEngineRunner.getWinningPlayer()){
                    winner = player;
                }
                winnerStringBuilder.append(player.getName() + "- score:" + player.getGamePlayer().getScore()
                                                + " health:" + player.getGamePlayer().getHealth() + "\n");
            }

            if (winner == null) {
                System.out.println("=======================================");
                System.out.println("The game ended in a tie");
                System.out.println("=======================================");
            }else{
                System.out.println("=======================================");
                System.out.println("The winner is: " + winner.getName());
                System.out.println("=======================================");
            }

            BufferedWriter bufferedWriter = null;
            try {
                String roundLocation = String.format("%s\\%s\\endGameState.txt", gameName, FileUtils.getRoundDirectory(gameMap.getCurrentRound() - 1));
                bufferedWriter = new BufferedWriter(new FileWriter(new File(roundLocation)));

                winnerStringBuilder.insert(0,"The winner is: " + winner.getName() + "\n\n");

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
            System.out.println("=======================================");
            System.out.println("Starting round " + round);
            System.out.println("=======================================");
        };
    }

    private Consumer<GameMap> getGameStartedHandler() {
        return gameMap -> {
            System.out.println("=======================================");
            System.out.println("Starting game");
            System.out.println("=======================================");
        };
    }
}
