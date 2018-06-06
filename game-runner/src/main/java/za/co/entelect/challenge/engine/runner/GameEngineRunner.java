package za.co.entelect.challenge.engine.runner;

import io.reactivex.subjects.BehaviorSubject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.util.TriConsumer;
import za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer;
import za.co.entelect.challenge.engine.exceptions.InvalidRunnerState;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.exceptions.MatchFailedException;
import za.co.entelect.challenge.game.contracts.exceptions.TimeoutException;
import za.co.entelect.challenge.game.contracts.game.GameEngine;
import za.co.entelect.challenge.game.contracts.game.GameMapGenerator;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.game.GameRoundProcessor;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.regex.MatchResult;

public class GameEngineRunner {

    private static final Logger log = LogManager.getLogger(GameEngineRunner.class);

    public Consumer<GameMap> firstPhaseHandler;
    public Function<GameMap, String> gameStartedHandler;
    public BiConsumer<GameMap, Integer> roundCompleteHandler;
    private String consoleOutput = "";
    private BehaviorSubject<String> addToConsoleOutput;
    private BehaviorSubject<Boolean> unsubscribe;
    public BiFunction<GameMap, Integer, String> roundStartingHandler;
    public TriConsumer<GameMap, List<Player>, Boolean> gameCompleteHandler;

    private GameMap gameMap;
    private List<Player> players;
    private RunnerRoundProcessor roundProcessor;

    private boolean gameComplete;

    private GameEngine gameEngine;
    private GameMapGenerator gameMapGenerator;
    private GameRoundProcessor gameRoundProcessor;

    public GameEngineRunner() {
        this.unsubscribe = BehaviorSubject.create();
        this.addToConsoleOutput = BehaviorSubject.create();
        this.addToConsoleOutput
                .takeUntil(this.unsubscribe)
                .subscribe(text -> consoleOutput += text);
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        this.unsubscribe.onNext(Boolean.TRUE);
    }

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

        if (gameMap == null) {
            throw new InvalidRunnerState("Game has not yet been prepared");
        }

        gameComplete = false;

        gameStartedHandler.apply(gameMap);
        startNewRound();

        runInitialPhase();
        while (!gameComplete) {
            processRound();
        }
    }

    private void runInitialPhase() throws Exception {
        boolean successfulRound = false;
        boolean botExceptionOccurred = false;

        while (!successfulRound) {

            for (Player player : players) {
                Thread thread = new Thread(() -> player.startGame(gameMap));
                thread.start();
                thread.join();
            }

            successfulRound = roundProcessor.processRound(addToConsoleOutput);
            players.forEach(p -> p.roundComplete(gameMap, gameMap.getCurrentRound()));

            if (!successfulRound) {
                roundProcessor.resetBackToStart();
                publishFirstPhaseFailed();
            }
        }
    }


    private void processRound() throws Exception {
        TowerDefenseConsoleMapRenderer renderer = new TowerDefenseConsoleMapRenderer();

        // Only execute the render if the log mode is in INFO.
        log.info(() -> {
            String consoleText = consoleOutput + renderer.render(gameMap, players.get(0).getGamePlayer());
            consoleOutput = "";

            return consoleText;
        });

        gameMap.setCurrentRound(gameMap.getCurrentRound() + 1);

        try {
            if (gameEngine.isGameComplete(gameMap)) {
                publishGameComplete(true);
                return;
            }
        } catch (TimeoutException e) {
            publishGameComplete(false);
            return;
        }

        startNewRound();
        for (Player player : players) {
            Thread thread = new Thread(() -> player.newRoundStarted(gameMap));
            thread.start();
            thread.join();
        }

        roundProcessor.processRound(addToConsoleOutput);
        players.forEach(p -> p.roundComplete(gameMap, gameMap.getCurrentRound()));
    }

    private void startNewRound() {
        roundProcessor = new RunnerRoundProcessor(gameMap, gameRoundProcessor);
        String newRoundText = roundStartingHandler.apply(gameMap, gameMap.getCurrentRound());
        addToConsoleOutput.onNext(newRoundText);
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

    private void publishGameComplete(boolean matchSuccessful) throws Exception {
        for (Player player : players) {
            player.gameEnded(gameMap);
        }

        gameComplete = true;
        GamePlayer winningPlayer = gameMap.getWinningPlayer();

        gameCompleteHandler.accept(gameMap, players, matchSuccessful);

        if (!matchSuccessful)
            throw new MatchFailedException("Match Failed");
    }

    private void publishFirstPhaseFailed() {
        firstPhaseHandler.accept(gameMap);
    }

    public GamePlayer getWinningPlayer() {
        return gameMap.getWinningPlayer();
    }
}

