package za.co.entelect.challenge.engine.runner;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer;
import za.co.entelect.challenge.engine.exceptions.InvalidRunnerState;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.game.GameEngine;
import za.co.entelect.challenge.game.contracts.game.GameMapGenerator;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.game.GameRoundProcessor;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;


public class GameEngineRunner {

    private static final Logger log = LogManager.getLogger(GameEngineRunner.class);

    public Consumer<GameMap> firstPhaseHandler;
    public Consumer<GameMap> gameStartedHandler;
    public BiConsumer<GameMap, Integer> roundCompleteHandler;
    public BiConsumer<GameMap, Integer> roundStartingHandler;
    public BiConsumer<GameMap, List<Player>> gameCompleteHandler;

    private GameMap gameMap;
    private List<Player> players;
    private RunnerRoundProcessor roundProcessor;

    private boolean gameComplete;

    private GameEngine gameEngine;
    private GameMapGenerator gameMapGenerator;
    private GameRoundProcessor gameRoundProcessor;

    public void preparePlayers(List<Player> players) throws InvalidRunnerState {

        if (players == null || players.size() == 0)
            throw new InvalidRunnerState("No players provided");

        this.players = players;
        for (Player player : players) {
            player.publishCommandHandler = getPlayerCommandListener();
        }
    }

    public void prepareGameMap() throws InvalidRunnerState {

        if (gameMapGenerator == null)
            throw new InvalidRunnerState("No GameMapGenerator instance found");

        if (players == null || players.size() == 0)
            throw new InvalidRunnerState("No players found");

        this.gameMap = gameMapGenerator.generateGameMap(players);
    }

    public void startNewGame() throws Exception {

        if (gameMap == null)
            throw new InvalidRunnerState("Game has not yet been prepared");

        gameComplete = false;

        gameStartedHandler.accept(gameMap);
        startNewRound();

        runInitialPhase();
        while (!gameComplete) {
            processRound();
        }
    }

    private void runInitialPhase() throws Exception {

        boolean successfulRound = false;
        while (!successfulRound) {

            for (Player player : players) {
                Thread thread = new Thread(() -> player.startGame(gameMap));
                thread.start();
                thread.join();
            }

            successfulRound = roundProcessor.processRound();

            for (Player player : players) {
                player.roundComplete(gameMap, gameMap.getCurrentRound());
            }

            if (!successfulRound) {
                roundProcessor.resetBackToStart();
                publishFirstPhaseFailed();
            }
        }
    }


    private void processRound() throws Exception {

        TowerDefenseConsoleMapRenderer renderer = new TowerDefenseConsoleMapRenderer();
        //Only execute the render if the log mode is in INFO.
        log.info(() -> renderer.render(gameMap, players.get(0).getGamePlayer()));

        gameMap.setCurrentRound(gameMap.getCurrentRound() + 1);

        if (gameEngine.isGameComplete(gameMap)) {
            publishGameComplete();
            return;
        }

        startNewRound();

        for (Player player : players) {
            Thread thread = new Thread(() -> player.newRoundStarted(gameMap));
            thread.start();
            thread.join();
        }

        roundProcessor.processRound();

        for (Player player : players) {
            player.roundComplete(gameMap, gameMap.getCurrentRound());
        }
    }

    private void startNewRound() {
        roundProcessor = new RunnerRoundProcessor(gameMap, gameRoundProcessor);
        roundStartingHandler.accept(gameMap, gameMap.getCurrentRound());
    }

    public void setGameMapGenerator(GameMapGenerator gameMapGenerator) {
        this.gameMapGenerator = gameMapGenerator;
    }

    public void setGameEngine(GameEngine gameEngine) {
        this.gameEngine = gameEngine;
    }

    public void setGameRoundProcessor(GameRoundProcessor gameRoundProcessor) {
        this.gameRoundProcessor = gameRoundProcessor;
    }

    private BiConsumer<Player, RawCommand> getPlayerCommandListener() {
        return (player, command) -> roundProcessor.addPlayerCommand(player, command);
    }

    private void publishGameComplete() {
        for (Player player : players) {
            player.gameEnded(gameMap);
        }

        gameComplete = true;
        GamePlayer winningPlayer = gameMap.getWinningPlayer();

        gameCompleteHandler.accept(gameMap, players);

        if (winningPlayer == null) {
            // TODO: game ended in a tie, how do we publish this to the tournament?
        }
    }

    private void publishFirstPhaseFailed() {
        firstPhaseHandler.accept(gameMap);
    }

    public GamePlayer getWinningPlayer(){
        return gameMap.getWinningPlayer();
    }
}

