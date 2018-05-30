package za.co.entelect.challenge.game.contracts.game;

import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.ArrayList;
import java.util.Map;

public interface GameRoundProcessor {

    boolean processRound(GameMap gameMap, Map<GamePlayer, RawCommand> commandsToProcess);

    ArrayList<String> getErrorList();
}
