package za.co.entelect.challenge.commands;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.game.contracts.command.Command;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

public class IronCurtainCommand implements Command {

    private static final Logger log = LogManager.getLogger(IronCurtainCommand.class);

    @Override
    public void performCommand(GameMap gameMap, GamePlayer gamePlayer) throws InvalidCommandException {
        TowerDefensePlayer currentPlayer = (TowerDefensePlayer) gamePlayer;
        TowerDefenseGameMap currentGameMap = (TowerDefenseGameMap) gameMap;

        if (!currentPlayer.canPlaceIronCurtain()) {
            int resetPeriod = GameConfig.getIroncurtainResetPeriod();
            logAndThrowException(
                    String.format("Cannot place the Iron Curtain consecutively, wait until [%d] rounds have elapsed",
                            resetPeriod - (currentGameMap.getCurrentRound() % resetPeriod)));

        } else if (currentPlayer.getEnergy() < GameConfig.getIroncurtainPrice()) {
            logAndThrowException(String.format("Not enough energy to setup the Iron Curtain, you need at least[%d] energy",
                    GameConfig.getIroncurtainPrice()));

        } else if (currentPlayer.getActiveIronCurtainTimeLeft() >= 1) {
            // Because the command is parsed right before the previous IronCurtain will be removed, use 1 (one) so that
            // players are allowed to place IronCurtains in a continuous time period.
            logAndThrowException(
                    String.format("You can only have one [1] active Iron Curtain at any time"));
        }

        currentGameMap.activateIronCurtain(currentPlayer);

        try {
            currentPlayer.removeEnergy(GameConfig.getIroncurtainPrice());
        } catch (Exception e) {
            log.error(e);
        }

    }

    private void logAndThrowException(String errorString) throws InvalidCommandException {
        log.error(errorString);
        throw new InvalidCommandException(errorString);
    }
}