package za.co.entelect.challenge.core.entities;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.BuildingStats;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.factories.BuildingFactory;

import java.util.Arrays;
import java.util.HashMap;

public class GameDetails {

    private int round;
    private int maxRounds;
    private int mapWidth;
    private int mapHeight;
    private int roundIncomeEnergy;

    @Deprecated
    private HashMap<BuildingType, Integer> buildingPrices = new HashMap<>();

    private HashMap<BuildingType, BuildingStats> buildingsStats = new HashMap<>();

    public GameDetails(int round) {
        this.round = round;
        this.maxRounds = GameConfig.getMaxRounds();
        this.mapWidth = GameConfig.getMapWidth();
        this.mapHeight = GameConfig.getMapHeight();
        this.roundIncomeEnergy = GameConfig.getRoundIncomeEnergy();

        Arrays.asList(BuildingType.values()).forEach(bt -> buildingPrices.put(bt, BuildingFactory.createBuildingStats(bt).price));

        Arrays.stream(BuildingType.values()).forEach(bt -> buildingsStats.put(bt, BuildingFactory.createBuildingStats(bt)));
    }
}
