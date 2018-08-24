package za.co.entelect.challenge.core.entities;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.BuildingStats;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.factories.BuildingFactory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class GameDetails {

    private int round;
    private int maxRounds;
    private int mapWidth;
    private int mapHeight;
    private int roundIncomeEnergy;

    @Deprecated
    private HashMap<BuildingType, Integer> buildingPrices = new HashMap<>();

    private HashMap<BuildingType, BuildingStats> buildingsStats = new HashMap<>();
    private HashMap<String, Integer> ironCurtainStats = new HashMap<>();

    public GameDetails(int round) {
        this.round = round;
        this.maxRounds = GameConfig.getMaxRounds();
        this.mapWidth = GameConfig.getMapWidth();
        this.mapHeight = GameConfig.getMapHeight();
        this.roundIncomeEnergy = GameConfig.getRoundIncomeEnergy();

        final List<BuildingType> buildingTypesWithoutStats = new ArrayList<BuildingType>() {{
            add(BuildingType.DECONSTRUCT);
            add(BuildingType.IRONCURTAIN);
        }};
        Arrays.stream(BuildingType.values())
                .filter(bt -> !buildingTypesWithoutStats.contains(bt))
                .forEach(bt -> buildingPrices.put(bt, BuildingFactory.createBuildingStats(bt).price));

        Arrays.stream(BuildingType.values())
                .filter(bt -> !buildingTypesWithoutStats.contains(bt))
                .forEach(bt -> buildingsStats.put(bt, BuildingFactory.createBuildingStats(bt)));

        ironCurtainStats.put("price", GameConfig.getIroncurtainPrice());
        ironCurtainStats.put("activeRounds", GameConfig.getIroncurtainActiveRounds());
        ironCurtainStats.put("resetPeriod", GameConfig.getIroncurtainResetPeriod());
        ironCurtainStats.put("constructionScore", GameConfig.getIroncurtainConstructionScore());
    }
}
