package za.co.entelect.challenge.commands;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.game.contracts.command.Command;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.Optional;

public class DeconstructBuildingCommand implements Command {

    private int positionX;
    private int positionY;

    private static final Logger log = LogManager.getLogger(DeconstructBuildingCommand.class);

    public DeconstructBuildingCommand(int positionX, int positionY) {
        this.positionX = positionX;
        this.positionY = positionY;
    }

    @Override
    public void performCommand(GameMap gameMap, GamePlayer gamePlayer) throws InvalidCommandException {
        TowerDefensePlayer currentPlayer = (TowerDefensePlayer) gamePlayer;
        TowerDefenseGameMap currentGameMap = (TowerDefenseGameMap) gameMap;

        if (!CommandHelpers.isCommandInBounds(positionX, positionY)) {
            String errorString = String.format("The position is out of bounds x:[%d] y:[%d]", positionX, positionY);
            log.error(errorString);
            throw new InvalidCommandException(errorString);
        }
        this.positionX = CommandHelpers.mirrorXIndex(currentPlayer.getPlayerType(), positionX);

        Optional<Building> buildingToDeconstruct = currentGameMap.getBuildings().stream()
                .filter(b -> b.getX() == positionX
                        && b.getY() == positionY)
                .findFirst();

        if (!buildingToDeconstruct.isPresent()) {
            throw new InvalidCommandException(String.format("No building to deconstruct, X:%s, Y:%s",
                    positionX, positionY));
        }

        currentGameMap.deconstructBuilding(buildingToDeconstruct.get());
        try {
            currentPlayer.addEnergy(GameConfig.getDeconstructionRefundAmount());
        } catch (Exception e) {
            log.error(e);
        }
    }
}