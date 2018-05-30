package za.co.entelect.challenge.engine.runner;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.engine.exceptions.InvalidOperationException;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.game.GameRoundProcessor;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

public class RunnerRoundProcessor {
    private static final Logger log = LogManager.getLogger(RunnerRoundProcessor.class);

    private GameMap gameMap;
    private GameRoundProcessor gameRoundProcessor;

    private boolean roundProcessed;

    RunnerRoundProcessor(GameMap gameMap, GameRoundProcessor gameRoundProcessor) {
        this.gameMap = gameMap;
        this.gameRoundProcessor = gameRoundProcessor;
    }

    boolean processRound(Map<GamePlayer, RawCommand> commands) throws Exception {
        if (roundProcessed) {
            throw new InvalidOperationException("This round has already been processed!");
        }

        boolean processed = gameRoundProcessor.processRound(gameMap, commands);
        ArrayList<String> errorList = gameRoundProcessor.getErrorList();
        //TODO: Remove later
        log.info("Error List: " + Arrays.toString(errorList.toArray()));
        roundProcessed = true;

        return processed;
    }

    void resetBackToStart() {

    }
}
