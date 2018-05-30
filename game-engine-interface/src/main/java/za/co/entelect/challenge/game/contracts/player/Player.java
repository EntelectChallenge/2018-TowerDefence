package za.co.entelect.challenge.game.contracts.player;

import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

/**
 * The base class for all players/test harnesses of the game to extend.
 * Processing of players happens in the following order.
 * 1. Player is registered to a game map, and PlayerRegistered is called.
 * 2. New Game is Started.
 * 3. Player Published a Command
 * 4. Player Waits for round to complete
 * 5. Round is Completed.
 * 6. Steps 3-5 until GameEnded is called
 * <p>
 * A player may only submit one command per round.  Any subsuquent commands will be ignored by the game runner. Exception may be thrown
 * A player may not submit commands to the game egine, any attempts to do so will be ignored by the game egnine. Exception may be thrown
 * <p>
 * The game runner places no restrictions on players, and expects each player to submit a command each round, while the player is alive.
 * <p>
 * If the test harness imposes any rules on the hosting player, such as a time limit, that restriction has to be enforced by the harness.
 * If the test harness hosts a player that can be instable, such as a network connection, or a malfunctioning bot, it must cater for that by
 * still submitting a do nothing command to the game runner every round until the game is ended, or the player is killed.
 * <p>
 * The player entity registered to the player is linked directly to the game runner, and should this NOT BE MODIFIED IN ANY WAY!  Yeah I can
 * make a protected entity, but we are in control of the players/test harnesses, so do not mess with the game egine by chaning the entity.
 * <p>
 * If you want to log information from the game runner for the player specifically, please provide a logger by setting the property, the game runner
 * will then pass game logs on to you player.
 */
public abstract class Player {

    private String name;

    private GamePlayer gamePlayer;

    public Player(String name) {
        this.name = name;
    }

    public void roundComplete(GameMap gameMap, int round) {
    }

    @Override
    public String toString() {
        return String.format("Player{name='%s'}", name);
    }

    public abstract RawCommand getPlayerCommand(GameMap gameMap);

    public abstract void gameEnded(GameMap gameMap);

    public abstract void playerKilled(GameMap gameMap);

    public abstract void playerCommandFailed(GameMap gameMap, String reason);

    public abstract void firstRoundFailed(GameMap gameMap, String reason);

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public GamePlayer getGamePlayer() {
        return gamePlayer;
    }

    public void setGamePlayer(GamePlayer gamePlayer) {
        this.gamePlayer = gamePlayer;
    }
}
