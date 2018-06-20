package za.co.entelect.challenge.commands;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.Optional;

public class DeconstructBuildingCommand extends RawCommand {

    private int positionX;
    private int positionY;
    private BuildingType buildingType;

    private static final Logger log = LogManager.getLogger(PlaceBuildingCommand.class);

    public DeconstructBuildingCommand(int positionX, int positionY, BuildingType buildingType) {
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
            String errorString = String.format("The position is out of bounds x:[%d] y:[%d]", positionX, positionY);
            log.error(errorString);
            throw new InvalidCommandException(errorString);
        }

        mirrorXIndex(currentPlayer.getPlayerType());

        Optional<Building> buildingToDeconstruct = currentGameMap.getBuildings().stream().filter(b ->
                b.getX() == positionX &&
                        b.getY() == positionY).findFirst();

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

    private void mirrorXIndex(PlayerType id) {
        if (id == PlayerType.B) {
            positionX = GameConfig.getMapWidth() - 1 - positionX;
        }
    }
}