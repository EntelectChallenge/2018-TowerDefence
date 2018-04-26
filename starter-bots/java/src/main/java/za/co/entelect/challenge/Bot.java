package za.co.entelect.challenge;

import za.co.entelect.challenge.entities.*;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;

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

    /**
     * Constructor
     *
     * @param gameState the game state
     **/
    public Bot(GameState gameState) {
        this.gameState = gameState;
        gameState.getGameMap();
        gameDetails = gameState.getGameDetails();
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

    /**
     * Run
     *
     * @return the result
     **/
    public String run() {
        if (isUnderAttack()) {
            return defendRow();
        } else if (hasEnoughEnergyForMostExpensiveBuilding()) {
            return buildRandom();
        } else {
            return doNothingCommand();
        }
    }

    /**
     * Build random building
     *
     * @return the result
     **/
    private String buildRandom() {
        List<CellStateContainer> emptyCells = gameState.getGameMap().stream()
                .filter(c -> c.getBuildings().size() == 0 && c.x < (gameWidth / 2))
                .collect(Collectors.toList());

        if (emptyCells.isEmpty()) {
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

    /**
     * Has enough energy for most expensive building
     *
     * @return the result
     **/
    private boolean hasEnoughEnergyForMostExpensiveBuilding() {
        return gameDetails.buildingPrices.values().stream()
                .filter(bp -> bp < myself.energy)
                .toArray().length == 3;
    }

    /**
     * Defend row
     *
     * @return the result
     **/
    private String defendRow() {
        for (int i = 0; i < gameHeight; i++) {
            int opponentAttacksCount = getAllBuildingsForPlayer(PlayerType.B, b -> b.buildingType == ATTACK, i).size();
            if (opponentAttacksCount > 0 && canAffordBuilding(DEFENSE)) {
                return placeBuildingInRow(DEFENSE, i);
            }
        }

        return buildRandom();
    }

    /**
     * Checks if this is under attack
     *
     * @return true if this is under attack
     **/
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

    /**
     * Do nothing command
     *
     * @return the result
     **/
    private String doNothingCommand() {
        return "";
    }

    /**
     * Place building in row
     *
     * @param buildingType the building type
     * @param y            the y
     * @return the result
     **/
    private String placeBuildingInRow(BuildingType buildingType, int y) {
        List<CellStateContainer> emptyCells = gameState.getGameMap().stream()
                .filter(c -> c.getBuildings().size() == 0
                        && c.y == y
                        && c.x < (gameWidth / 2) - 1)
                .collect(Collectors.toList());

        if (emptyCells.isEmpty()) {
            return buildRandom();
        }

        CellStateContainer randomEmptyCell = getRandomElementOfList(emptyCells);
        return buildCommand(randomEmptyCell.x, randomEmptyCell.y, buildingType);
    }

    /**
     * Get random element of list
     *
     * @param list the list < t >
     * @return the result
     **/
    private <T> T getRandomElementOfList(List<T> list) {
        return list.get((new Random()).nextInt(list.size()));
    }

    /**
     * Build command
     *
     * @param x            the x
     * @param y            the y
     * @param buildingType the building type
     * @return the result
     **/
    private String buildCommand(int x, int y, BuildingType buildingType) {
        return String.format("%d,%d,%d", x, y, buildingType.getType());
    }

    /**
     * Get all buildings for player
     *
     * @param playerType the player type
     * @param filter     the filter
     * @param y          the y
     * @return the result
     **/
    private List<Building> getAllBuildingsForPlayer(PlayerType playerType, Predicate<Building> filter, int y) {
        return buildings.stream()
                .filter(b -> b.getPlayerType() == playerType
                        && b.getY() == y)
                .filter(filter)
                .collect(Collectors.toList());
    }

    /**
     * Can afford building
     *
     * @param buildingType the building type
     * @return the result
     **/
    private boolean canAffordBuilding(BuildingType buildingType) {
        return myself.energy >= gameDetails.buildingPrices.get(buildingType);
    }
}
