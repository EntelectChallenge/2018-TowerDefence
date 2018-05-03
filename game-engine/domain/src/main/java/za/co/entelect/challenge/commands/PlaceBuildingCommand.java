package za.co.entelect.challenge.commands;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.factories.BuildingFactory;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

public class PlaceBuildingCommand extends RawCommand {

    private int positionX;
    private int positionY;
    private BuildingType buildingType;

    public PlaceBuildingCommand(int positionX, int positionY, BuildingType buildingType) {
        this.positionX = positionX;
        this.positionY = positionY;
        this.buildingType = buildingType;
    }

    @Override
    public void performCommand(GameMap gameMap, GamePlayer gamePlayer) throws InvalidCommandException {
        TowerDefensePlayer currentPlayer = (TowerDefensePlayer) gamePlayer;
        TowerDefenseGameMap currentGameMap = (TowerDefenseGameMap) gameMap;

        int mapWidth = GameConfig.getMapWidth();
        int mapHeight = GameConfig.getMapHeight();

        if ((positionX >= (mapWidth / 2) || positionX < 0) || (positionY >= (mapHeight) || positionY < 0)) {
            throw new InvalidCommandException(String.format("The position is out of bounds x:[%d] y:[%d]", positionX, positionY));
        }

        mirrorXIndex(currentPlayer.getPlayerType());

        Building buildingToAdd = BuildingFactory.createBuilding(positionX, positionY, buildingType, currentPlayer.getPlayerType());

        boolean isOccupiedCell = currentGameMap.getBuildings().stream().anyMatch(b ->
                b.getX() == positionX && b.getY() == positionY
        );

        if (isOccupiedCell) {
            throw new InvalidCommandException(String.format("Cannot build a building on a cell occupied by another building, X:%s, Y:%s",
                    buildingToAdd.getX(), buildingToAdd.getY()));
        }

        if (currentPlayer.getEnergy() >= buildingToAdd.getPrice()) {
            currentGameMap.addBuilding(buildingToAdd);
            try {
                currentPlayer.removeEnergy(buildingToAdd.getPrice());
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            throw new InvalidCommandException(String.format("You don't have enough energy to build this building. Required:[%d], Current:[%d]",
                    buildingToAdd.getPrice(), currentPlayer.getEnergy()));
        }
    }

    private void mirrorXIndex(PlayerType id) {
        if (id == PlayerType.B) {
            positionX = GameConfig.getMapWidth() - 1 - positionX;
        }
    }
}
