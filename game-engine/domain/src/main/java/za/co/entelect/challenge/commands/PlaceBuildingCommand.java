package za.co.entelect.challenge.commands;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.factories.BuildingFactory;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

public class PlaceBuildingCommand extends RawCommand {

    private int positionX;
    private int positionY;
    private BuildingType buildingType;

    private static final Logger log = LogManager.getLogger(PlaceBuildingCommand.class);

    public PlaceBuildingCommand(int positionX, int positionY, BuildingType buildingType) {
        this.positionX = positionX;
        this.positionY = positionY;
        this.buildingType = buildingType;
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

        if (buildingType.equals(BuildingType.TESLA)) {
            if (currentGameMap.getBuildings().stream()
                    .filter(building -> building.getBuildingType().equals(BuildingType.TESLA)
                            && building.getPlayerType().equals(currentPlayer.getPlayerType()))
                    .count() >= 2) {
                throw new InvalidCommandException("No more than 2 tesla towers allowed per player");
            }
        }

        Building buildingToAdd = BuildingFactory.createBuilding(positionX, positionY, buildingType, currentPlayer.getPlayerType());

        boolean isOccupiedCell = currentGameMap.getBuildings().stream()
                .anyMatch(b -> b.getX() == positionX && b.getY() == positionY);

        if (isOccupiedCell) {
            throw new InvalidCommandException(String.format("Cannot build a building on a cell occupied by another building, X:%s, Y:%s",
                    buildingToAdd.getX(), buildingToAdd.getY()));
        }

        if (currentPlayer.getEnergy() >= buildingToAdd.getPrice()) {
            currentGameMap.addBuilding(buildingToAdd);
            try {
                currentPlayer.removeEnergy(buildingToAdd.getPrice());
            } catch (Exception e) {
                log.error(e);
            }
        } else {
            throw new InvalidCommandException(String.format("You don't have enough energy to build this building. Required:[%d], Current:[%d]",
                    buildingToAdd.getPrice(), currentPlayer.getEnergy()));
        }
    }
}
