package za.co.entelect.challenge;

import za.co.entelect.challenge.entities.*;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;

import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static za.co.entelect.challenge.enums.BuildingType.*;

public class Bot {

    private GameState gameState;
    private GameDetails gameDetails;
    private int gameWidth;
    private int gameHeight;
    private Player myself;
    private Player opponent;
    private List<Building> buildings;
    private List<Missile> missiles;

    public Bot(GameState gameState) {
        this.gameState = gameState;
        gameState.getGameMap();
        gameDetails = gameState.getGameDetails();
//        energyPerTurn = gameDetails.;
        gameWidth = gameDetails.mapWidth;
        gameHeight = gameDetails.mapHeight;
        myself = gameState.getPlayers().stream().filter(p -> p.playerType == PlayerType.A).findFirst().get();
        opponent = gameState.getPlayers().stream().filter(p -> p.playerType == PlayerType.B).findFirst().get();

        buildings = gameState.getGameMap().stream()
                .flatMap(c -> c.getBuildings().stream())
                .collect(Collectors.toList());

        missiles = gameState.getGameMap().stream()
                .flatMap(c -> c.getMissiles().stream())
                .collect(Collectors.toList());

    }

    public String Run() {
        if (isUnderAttack()) {
            return defendRow();
        } else if (hasEnoughEnergyForMostExpensiveBuilding()) {
            return buildRandom();
        } else {
            return doNothingCommand();
        }
    }


    private String buildRandom() {
        List<CellStateContainer> emptyCells = gameState.getGameMap().stream()
                .filter(c -> c.getBuildings().size() == 0 && c.x < (gameWidth / 2))
                .collect(Collectors.toList());
        if (emptyCells.size() == 0) {
            return doNothingCommand();
        }
        CellStateContainer randomEmptyCell = getRandomElementOfList(emptyCells);
        List<BuildingType> buildingTypes = new ArrayList<>(gameDetails.buildingPrices.keySet());
        BuildingType randomBuildingType = getRandomElementOfList(buildingTypes);
        if (!canAffordBuilding(randomBuildingType)) {
            return doNothingCommand();
        }
        return buildCommand(randomEmptyCell.x, randomEmptyCell.y, randomBuildingType);
    }

    private boolean hasEnoughEnergyForMostExpensiveBuilding() {
        boolean hasEnoughEnergy = gameDetails.buildingPrices.values().stream()
                .filter(bp -> bp < myself.energy)
                .toArray().length == 3;
        return hasEnoughEnergy;
    }

    private String defendRow() {
        for (int i = 0; i < gameHeight; i++) {
            int opponentAttacksCount = getAllBuildingsForPlayer(PlayerType.B, b -> b.buildingType == ATTACK, i).size();
            if (opponentAttacksCount > 0 && canAffordBuilding(DEFENSE)) {
                return placeBuildingInRow(DEFENSE, i);
            }
        }

        return buildRandom();
    }

    private boolean isUnderAttack() {
        //if enemy has an attack building and i dont have a blocking wall
        for (int i = 0; i < gameHeight; i++) {
            int opponentAttacksCount = getAllBuildingsForPlayer(PlayerType.B, b -> b.buildingType == ATTACK, i).size();
            int myDefenseCount = getAllBuildingsForPlayer(PlayerType.A, b -> b.buildingType == DEFENSE, i).size();
            if (opponentAttacksCount > 0 && myDefenseCount == 0) {
                return true;
            }
        }
        return false;
    }

    private String doNothingCommand() {
        return "";
    }

    private String placeBuildingInRow(BuildingType buildingType, int y) {
        List<CellStateContainer> emptyCells = gameState.getGameMap().stream()
                .filter(c -> c.getBuildings().size() == 0
                        && c.y == y
                        && c.x < (gameWidth / 2) - 1)
                .collect(Collectors.toList());
        if (emptyCells.size() == 0) {
            return buildRandom();
        }
        CellStateContainer randomEmptyCell = getRandomElementOfList(emptyCells);
        return buildCommand(randomEmptyCell.x, randomEmptyCell.y, buildingType);
    }

    private <T> T getRandomElementOfList(List<T> list) {
        return list.get((new Random()).nextInt(list.size()));
    }

    private String buildCommand(int x, int y, BuildingType buildingType) {
        StringBuilder buildCommand = new StringBuilder();

        buildCommand.append(x);
        buildCommand.append(",");
        buildCommand.append(y);
        buildCommand.append(",");
        switch (buildingType) {

            case DEFENSE:
                buildCommand.append("0");
                break;
            case ATTACK:
                buildCommand.append("1");
                break;
            case ENERGY:
                buildCommand.append("2");
                break;
        }
        return buildCommand.toString();
    }

    private List<Building> getAllBuildingsForPlayer(PlayerType playerType, Predicate<Building> filter, int y) {
        return buildings.stream()
                .filter(b -> b.getPlayerType() == playerType
                        && b.getY() == y)
                .filter(filter)
                .collect(Collectors.toList());
    }

    private boolean canAffordBuilding(BuildingType buildingType) {
        int cost = 0;
        switch (buildingType) {
            case DEFENSE:
                cost = gameDetails.buildingPrices.get(DEFENSE);
                break;
            case ATTACK:
                cost = gameDetails.buildingPrices.get(ATTACK);
                break;
            case ENERGY:
                cost = gameDetails.buildingPrices.get(ENERGY);
                break;
        }

        return myself.energy >= cost;
    }

}
