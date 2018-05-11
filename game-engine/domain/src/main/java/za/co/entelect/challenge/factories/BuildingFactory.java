package za.co.entelect.challenge.factories;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.BuildingStats;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;

public class BuildingFactory {

    public static Building createBuilding(int x, int y, BuildingType buildingType, PlayerType playerType) {

        Building building = new Building(x, y, playerType,
                GameConfig.getDefenseHealth(),
                GameConfig.getDefenseConstructionTimeLeft(),
                GameConfig.getDefensePrice(),
                GameConfig.getDefenseWeaponDamage(),
                GameConfig.getDefenseWeaponSpeed(),
                GameConfig.getDefenseWeaponCooldownPeriod(),
                GameConfig.getDefenseIcon(),
                GameConfig.getDefenseDestroyMultiplier(),
                GameConfig.getDefenseConstructionScore(),
                GameConfig.getDefenseEnergyPerTurn(),
                BuildingType.DEFENSE);

        if (buildingType == BuildingType.ATTACK) {
            building = new Building(x, y, playerType,
                    GameConfig.getAttackHealth(),
                    GameConfig.getAttackConstructionTimeLeft(),
                    GameConfig.getAttackPrice(),
                    GameConfig.getAttackWeaponDamage(),
                    GameConfig.getAttackWeaponSpeed(),
                    GameConfig.getAttackWeaponCooldownPeriod(),
                    GameConfig.getAttackIcon(),
                    GameConfig.getAttackDestroyMultiplier(),
                    GameConfig.getAttackConstructionScore(),
                    GameConfig.getAttackEnergyPerTurn(),
                    BuildingType.ATTACK);
        }

        if (buildingType == BuildingType.ENERGY) {
            building = new Building(x, y, playerType,
                    GameConfig.getEnergyHealth(),
                    GameConfig.getEnergyConstructionTimeLeft(),
                    GameConfig.getEnergyPrice(),
                    GameConfig.getEnergyWeaponDamage(),
                    GameConfig.getEnergyWeaponSpeed(),
                    GameConfig.getEnergyWeaponCooldownPeriod(),
                    GameConfig.getEnergyIcon(),
                    GameConfig.getEnergyDestroyMultiplier(),
                    GameConfig.getEnergyConstructionScore(),
                    GameConfig.getEnergyEnergyPerTurn(),
                    BuildingType.ENERGY);
        }

        return building;
    }

    public static BuildingStats createBuildingStats(BuildingType buildingType) {
        Building building = createBuilding(0, 0, buildingType, PlayerType.A);
        return new BuildingStats(building);
    }

}
