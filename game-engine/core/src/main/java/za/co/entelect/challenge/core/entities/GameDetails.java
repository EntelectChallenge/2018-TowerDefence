package za.co.entelect.challenge.core.entities;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.enums.BuildingType;

import java.util.HashMap;

public class GameDetails {
    private int round;
    private int mapWidth;
    private int mapHeight;
    private HashMap<BuildingType, Integer> buildingPrices;

    public GameDetails(int round){
        this.round = round;
        this.mapWidth = GameConfig.getMapWidth();
        this.mapHeight = GameConfig.getMapHeight();

        buildingPrices = new HashMap<>();
        buildingPrices.put(BuildingType.DEFENSE, GameConfig.getDefensePrice());
        buildingPrices.put(BuildingType.ATTACK, GameConfig.getAttackPrice());
        buildingPrices.put(BuildingType.ENERGY, GameConfig.getEnergyPrice());
    }
}
