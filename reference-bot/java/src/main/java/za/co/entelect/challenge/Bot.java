package za.co.entelect.challenge;

import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.CellStateContainer;
import za.co.entelect.challenge.entities.GameState;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;

import java.time.LocalTime;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class Bot {
    private GameState gameState;
    private Random rand = new Random(LocalTime.now().toNanoOfDay());

    public Bot(GameState gameState){
        this.gameState = gameState;
        gameState.getGameMap();
    }

    public String Run(){
        String command = "";

        //if enemy has an attack building and i dont have a blocking wall, then block from front
        for (int i = 0; i < gameState.gameDetails.mapHeight; i++){
            int enemyAttackOnRow = getAllBuildingsForPlayer(PlayerType.B, b -> b.buildingType == BuildingType.ATTACK, i).size();
            int myDefenseOnRow = getAllBuildingsForPlayer(PlayerType.A, b -> b.buildingType == BuildingType.DEFENSE, i).size();

            if (enemyAttackOnRow > 0 && myDefenseOnRow == 0){
                if ( canAffordBuilding(BuildingType.DEFENSE))
                    command = placeBuildingInRowFromFront(BuildingType.DEFENSE, i);
                else
                    command = "";
                break;
            }
        }

        //if there is a row where i don't have energy and there is no enemy attack build energy in the back row
        if (command.equals("")) {
            for (int i = 0; i < gameState.gameDetails.mapHeight; i++) {
                int enemyAttackOnRow = getAllBuildingsForPlayer(PlayerType.B, b -> b.buildingType == BuildingType.ATTACK, i).size();
                int myEnergyOnRow = getAllBuildingsForPlayer(PlayerType.A, b -> b.buildingType == BuildingType.ENERGY, i).size();

                if (enemyAttackOnRow == 0 && myEnergyOnRow == 0 ) {
                    if (canAffordBuilding(BuildingType.ENERGY))
                        command = placeBuildingInRowFromBack(BuildingType.ENERGY, i);
                    break;
                }
            }
        }

        //if i have a defense building on a row, then build an attack building behind it.
        if (command.equals("")){
            for (int i = 0; i < gameState.gameDetails.mapHeight; i++) {
                if ( getAllBuildingsForPlayer(PlayerType.A, b -> b.buildingType == BuildingType.DEFENSE, i).size() > 0
                        && canAffordBuilding(BuildingType.ATTACK)){
                    command = placeBuildingInRowFromFront(BuildingType.ATTACK, i);
                }
            }
        }

        //if i don't need to do anything then do either attack or defend randomly based on chance (65% attack, 35% defense)
        if (command.equals("")){
            if (getEnergy(PlayerType.A) >= getMostExpensiveBuildingPrice()){
                if (rand.nextInt(100) <= 35){
                    return placeBuildingRandomlyFromFront(BuildingType.DEFENSE);
                }else{
                    return placeBuildingRandomlyFromBack(BuildingType.ATTACK);
                }
            }
        }

        return command;
    }

    private String placeBuildingRandomlyFromBack(BuildingType buildingType){
        for (int i = 0; i < gameState.gameDetails.mapWidth/ 2; i ++){
            List<CellStateContainer> listOfFreeCells = getListOfEmptyCellsForColumn(i);
            if (listOfFreeCells.size() != 0){
                CellStateContainer pickedCell = listOfFreeCells.get(rand.nextInt(listOfFreeCells.size()));
                return buildCommand(pickedCell.x, pickedCell.y, buildingType);
            }
        }
        return "";
    }

    private String placeBuildingRandomlyFromFront(BuildingType buildingType){
        for (int i = (gameState.gameDetails.mapWidth / 2) - 1; i >= 0; i--){
            List<CellStateContainer> listOfFreeCells = getListOfEmptyCellsForColumn(i);
            if (listOfFreeCells.size() != 0){
                CellStateContainer pickedCell = listOfFreeCells.get(rand.nextInt(listOfFreeCells.size()));
                return buildCommand(pickedCell.x, pickedCell.y, buildingType);
            }
        }
        return "";
    }

    private String placeBuildingInRowFromFront(BuildingType buildingType, int y){
        for (int i = (gameState.gameDetails.mapWidth / 2) - 1; i >= 0; i--){
            if (isCellEmpty(i, y)){
                return buildCommand(i, y, buildingType);
            }
        }
        return "";
    }

    private String placeBuildingInRowFromBack(BuildingType buildingType, int y){
        for (int i = 0; i < gameState.gameDetails.mapWidth / 2; i++){
            if (isCellEmpty(i, y)){
                return buildCommand(i, y, buildingType);
            }
        }
        return "";
    }

    private String buildCommand(int x, int y, BuildingType buildingType){
        return String.format("%s,%d,%s", String.valueOf(x), y, buildingType.getCommandCode());
    }

    private List<Building> getAllBuildingsForPlayer(PlayerType playerType, Predicate<Building> filter, int y){
        return gameState.getGameMap().stream()
                .filter(c -> c.cellOwner == playerType && c.y == y)
                .flatMap(c -> c.getBuildings().stream())
                .filter(filter)
                .collect(Collectors.toList());
    }

    private List<CellStateContainer> getListOfEmptyCellsForColumn(int x){
        return gameState.getGameMap().stream()
                .filter(c -> c.x == x && isCellEmpty(x, c.y))
                .collect(Collectors.toList());
    }

    private boolean isCellEmpty(int x, int y) {
        Optional<CellStateContainer> cellOptional = gameState.getGameMap().stream()
                .filter(c -> c.x == x && c.y == y)
                .findFirst();

        if (cellOptional.isPresent()){
            CellStateContainer cell = cellOptional.get();
            return cell.getBuildings().size() <= 0;
        }else{
            System.out.println("Invalid cell selected");
        }
        return true;
    }

    private boolean canAffordBuilding(BuildingType buildingType){
        return getEnergy(PlayerType.A) >= getPriceForBuilding(buildingType);
    }

    private int getEnergy(PlayerType playerType){
        return gameState.getPlayers().stream()
                .filter(p -> p.playerType == playerType)
                .mapToInt(p -> p.energy)
                .sum();
    }

    private int getPriceForBuilding(BuildingType buildingType){
        return gameState.gameDetails.buildingPrices.get(buildingType);
    }

    private int getMostExpensiveBuildingPrice(){
        int buildingPrice = 0;
        for (Integer value : gameState.gameDetails.buildingPrices.values()){
            if (buildingPrice == 0){
                buildingPrice = value;
            }
            if (value > buildingPrice){
                buildingPrice = value;
            }
        }
        return buildingPrice;
    }
}
