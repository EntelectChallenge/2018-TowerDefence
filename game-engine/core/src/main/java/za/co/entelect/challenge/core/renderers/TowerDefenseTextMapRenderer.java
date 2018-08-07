package za.co.entelect.challenge.core.renderers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.core.entities.CellStateContainer;
import za.co.entelect.challenge.entities.*;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.factories.BuildingFactory;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;

public class TowerDefenseTextMapRenderer implements GameMapRenderer {

    private TowerDefenseGameMap tdMap;

    private static final Logger log = LogManager.getLogger(TowerDefenseTextMapRenderer.class);

    @Override
    public String render(GameMap gameMap, GamePlayer gamePlayer) {
        tdMap = (TowerDefenseGameMap) gameMap;
        PlayerType playerType = ((TowerDefensePlayer) gamePlayer).getPlayerType();

        StringBuilder stringBuilder = new StringBuilder();

        stringBuilder.append("XXXXXXXXX GAME INFO XXXXXXXXX\n");
        stringBuilder.append("Round Number : ").append(gameMap.getCurrentRound()).append("\n");
        stringBuilder.append("Maximum Amount Of Rounds : ").append(GameConfig.getMaxRounds()).append("\n");
        stringBuilder.append("Map Width : ").append(GameConfig.getMapWidth()).append("\n");
        stringBuilder.append("Map Height : ").append(GameConfig.getMapHeight()).append("\n");
        stringBuilder.append("Round Income Energy : ").append(GameConfig.getRoundIncomeEnergy()).append("\n");
        stringBuilder.append("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");
        stringBuilder.append("\n");

        stringBuilder.append("****** BUILDING STATS ******\n");
        stringBuilder.append("type;").append(BuildingStats.getTextHeader()).append("\n");
        stringBuilder.append("ATTACK;").append(BuildingFactory.createBuildingStats(BuildingType.ATTACK)).append("\n");
        stringBuilder.append("DEFENSE;").append(BuildingFactory.createBuildingStats(BuildingType.DEFENSE)).append("\n");
        stringBuilder.append("ENERGY;").append(BuildingFactory.createBuildingStats(BuildingType.ENERGY)).append("\n");
        stringBuilder.append("TESLA;").append(BuildingFactory.createBuildingStats(BuildingType.TESLA)).append("\n");
        stringBuilder.append("*****************************\n");
        stringBuilder.append("\n");

        stringBuilder.append("***** IRON CURTAIN STATS ****\n");
        stringBuilder.append("price;activeRounds;resetPeriod;constructionScore").append("\n");
        stringBuilder
                .append(GameConfig.getIroncurtainPrice()).append(";")
                .append(GameConfig.getIroncurtainActiveRounds()).append(";")
                .append(GameConfig.getIroncurtainResetPeriod()).append(";")
                .append(GameConfig.getIroncurtainConstructionScore()).append(";")
                .append("\n");
        stringBuilder.append("*****************************\n");
        stringBuilder.append("\n");

        TowerDefensePlayer playerA = null;
        TowerDefensePlayer playerB = null;
        try {
            if (playerType == PlayerType.A) {
                playerA = tdMap.getPlayer(PlayerType.A);
                playerB = tdMap.getPlayer(PlayerType.B);
            } else {
                playerA = tdMap.getPlayer(PlayerType.B);
                playerB = tdMap.getPlayer(PlayerType.A);
            }
        } catch (Exception e) {
            log.error(e);
        }
        appendPlayerDetails(stringBuilder, playerA, "A");
        appendPlayerDetails(stringBuilder, playerB, "B");

        stringBuilder.append("############# MAP #############\n");
        CellStateContainer[][] cellStateMap;
        if (playerType == PlayerType.A) {
            cellStateMap = RendererHelper.renderPlayerA(tdMap);
        } else {
            cellStateMap = RendererHelper.renderPlayerB(tdMap);
        }

        for (int y = 0; y < GameConfig.getMapHeight(); y++) {
            CellStateContainer[] row = cellStateMap[y];
            stringBuilder.append(getRowStringForPlayer(row, y));
            stringBuilder.append("\n");
        }

        stringBuilder.append("###############################\n");
        stringBuilder.append('\n');

        stringBuilder.append("######## BUILDING DATA #########\n");
        stringBuilder.append("FORMAT : [x,y] Owner|BuildingType|ConstructionTimeLeft|Health|WeaponCooldownTimeLeft|WeaponDamage|EnergyGeneratedPerTurn \n\n");

        for (Building building : tdMap.getBuildings()) {
            Building presentedBuilding = building;
            if (playerType == PlayerType.B) {
                presentedBuilding = presentedBuilding.getInvertedXInstance();
            }

            stringBuilder.append("[").append(presentedBuilding.getX()).append(",").append(presentedBuilding.getY()).append("] ");
            stringBuilder.append(presentedBuilding.getPlayerType()).append("|");
            stringBuilder.append(presentedBuilding.getBuildingType()).append("|");
            stringBuilder.append(presentedBuilding.getConstructionTimeLeft()).append("|");
            stringBuilder.append(presentedBuilding.getHealth()).append("|");
            stringBuilder.append(presentedBuilding.getWeaponCooldownTimeLeft()).append("|");
            stringBuilder.append(presentedBuilding.getWeaponDamage()).append("|");
            stringBuilder.append(presentedBuilding.getEnergyGeneratedPerTurn()).append("\n");
        }
        stringBuilder.append("###############################\n");
        stringBuilder.append('\n');

        stringBuilder.append("####### MISSILE DATA ########\n");

        stringBuilder.append("FORMAT : [x,y] Owner|Damage|Speed \n\n");

        for (Missile missile : tdMap.getMissiles()) {
            Missile presentedMissile = missile;
            if (playerType == PlayerType.B) {
                presentedMissile = presentedMissile.getInvertedXInstance();
            }

            stringBuilder.append("[").append(presentedMissile.getX()).append(",").append(presentedMissile.getY()).append("] ");
            stringBuilder.append(presentedMissile.getPlayerType()).append("|");
            stringBuilder.append(presentedMissile.getDamage()).append("|");
            stringBuilder.append(presentedMissile.getSpeed()).append("\n");
        }
        stringBuilder.append("###############################\n");

        return stringBuilder.toString();
    }

    private void appendPlayerDetails(StringBuilder stringBuilder, TowerDefensePlayer player, String intededPlayerId) {
        stringBuilder.append("---------- PLAYER ").append(intededPlayerId).append(" ----------\n");
        stringBuilder.append("Energy : ").append(player.getEnergy()).append("\n");
        stringBuilder.append("Health : ").append(player.getHealth()).append("\n");
        stringBuilder.append("HitsTaken : ").append(player.getHitsTaken()).append("\n");
        stringBuilder.append("Score : ").append(player.getScore()).append("\n");
        stringBuilder.append("IronCurtainAvailable : ").append(booleanToString(player.canPlaceIronCurtain())).append("\n");
        stringBuilder.append("ActiveIronCurtainLifetime : ").append(player.getActiveIronCurtainTimeLeft()).append("\n");
        stringBuilder.append("------------------------------\n");
        stringBuilder.append("\n");
    }

    private static String booleanToString(boolean b) {
        return b ? "1" : "0";
    }

    private String getRowStringForPlayer(CellStateContainer[] row, int y) {
        StringBuilder stringBuilderRow = new StringBuilder();

        for (int x = 0; x < row.length; x++) {
            stringBuilderRow.append("[");
            stringBuilderRow.append(x).append(",").append(y).append(",");

            if (row[x].buildings.size() > 0) {
                Building building = row[x].buildings.get(0);
                stringBuilderRow.append(building.isConstructed()
                        ? building.getIcon().toUpperCase()
                        : building.getIcon().toLowerCase());
            } else {
                stringBuilderRow.append("N");
            }

            stringBuilderRow.append(",");
            long playerAMissileCount = row[x].missiles.stream()
                    .filter(p -> p.getPlayerType() == PlayerType.A)
                    .count();
            long playerBMissileCount = row[x].missiles.stream()
                    .filter(p -> p.getPlayerType() == PlayerType.B)
                    .count();

            stringBuilderRow.append(playerAMissileCount).append(",").append(playerBMissileCount);
            stringBuilderRow.append("]");
        }

        return stringBuilderRow.toString();
    }

    @Override
    public String commandPrompt(GamePlayer gamePlayer) {
        return "";
    }

}
