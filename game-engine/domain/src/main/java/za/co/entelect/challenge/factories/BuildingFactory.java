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
                GameConfig.getDefenseRange(),
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
                    GameConfig.getAttackRange(),
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
                    GameConfig.getEnergyRange(),
                    BuildingType.ENERGY);
        }

        if (buildingType == BuildingType.TESLA) {
            building = new Building(x, y, playerType,
                    GameConfig.getTeslaHealth(),
                    GameConfig.getTeslaConstructionTimeLeft(),
                    GameConfig.getTeslaPrice(),
                    GameConfig.getTeslaWeaponDamage(),
                    GameConfig.getTeslaWeaponSpeed(),
                    GameConfig.getTeslaWeaponCooldownPeriod(),
                    GameConfig.getTeslaIcon(),
                    GameConfig.getTeslaDestroyMultiplier(),
                    GameConfig.getTeslaConstructionScore(),
                    GameConfig.getTeslaEnergyPerTurn(),
                    GameConfig.getTeslaRange(),
                    GameConfig.getTeslaWeaponEnergyPerShot(),
                    BuildingType.TESLA);
        }

        return building;
    }

    public static BuildingStats createBuildingStats(BuildingType buildingType) {
        Building building = createBuilding(0, 0, buildingType, PlayerType.A);
        return new BuildingStats(building);
    }

}
