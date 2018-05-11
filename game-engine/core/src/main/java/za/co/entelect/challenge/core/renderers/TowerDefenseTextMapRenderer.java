package za.co.entelect.challenge.core.renderers;

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
    @Override
    public String render(GameMap gameMap, GamePlayer gamePlayer) {
        tdMap = (TowerDefenseGameMap) gameMap;
        PlayerType playerType = ((TowerDefensePlayer) gamePlayer).getPlayerType();

        StringBuilder stringBuilder = new StringBuilder();

        stringBuilder.append("XXXXXXXXX GAME INFO XXXXXXXXX\n");
        stringBuilder.append("Round Number : " + gameMap.getCurrentRound() + "\n");
        stringBuilder.append("Map Width : " + GameConfig.getMapWidth() + "\n");
        stringBuilder.append("Map Height : " + GameConfig.getMapHeight() + "\n");
        stringBuilder.append("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n");
        stringBuilder.append("\n");

        stringBuilder.append("****** BUILDING STATS ******\n");
        stringBuilder.append("type;" + BuildingStats.getTextHeader() + "\n");
        stringBuilder.append("ATTACK;" + BuildingFactory.createBuildingStats(BuildingType.ATTACK) + "\n");
        stringBuilder.append("DEFENSE;" + BuildingFactory.createBuildingStats(BuildingType.DEFENSE) + "\n");
        stringBuilder.append("ENERGY;" + BuildingFactory.createBuildingStats(BuildingType.ENERGY) + "\n");
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
            e.printStackTrace();
        }

        stringBuilder.append("---------- PLAYER A ----------\n");
        stringBuilder.append("Energy : " + playerA.getEnergy() + "\n");
        stringBuilder.append("Health : " + playerA.getHealth() + "\n");
        stringBuilder.append("HitsTaken : " + playerA.getHitsTaken() + "\n");
        stringBuilder.append("Score : " + playerA.getScore() + "\n");
        stringBuilder.append("------------------------------\n");
        stringBuilder.append("\n");

        stringBuilder.append("---------- PLAYER B ----------\n");
        stringBuilder.append("Energy : " + playerB.getEnergy() + "\n");
        stringBuilder.append("Health : " + playerB.getHealth() + "\n");
        stringBuilder.append("HitsTaken : " + playerB.getHitsTaken() + "\n");
        stringBuilder.append("Score : " + playerB.getScore() + "\n");
        stringBuilder.append("------------------------------\n");
        stringBuilder.append("\n");


        stringBuilder.append("############# MAP #############\n");
        CellStateContainer[][] cellStateMap = null;
        if (playerType == PlayerType.A) {
            cellStateMap = RendererHelper.renderPlayerA(tdMap);
        } else {
            cellStateMap = RendererHelper.renderPlayerB(tdMap);
        }

        for (int y = 0; y < GameConfig.getMapHeight(); y++){
            CellStateContainer[] row = cellStateMap[y];
            stringBuilder.append(getRowStringForPlayer(row, y));
            stringBuilder.append("\n");
        }

        stringBuilder.append("###############################\n");
        stringBuilder.append('\n');

        stringBuilder.append("######## BUILDING DATA #########\n");

        stringBuilder.append("FORMAT : [x,y] Owner|ConstructionTimeLeft|Health|WeaponCooldownTimeLeft|WeaponDamage|EnergyGeneratedPerTurn \n\n");

        for (Building building :
            tdMap.getBuildings()) {
            Building presentedBuilding  = building;
            if (playerType == PlayerType.B) {
                presentedBuilding = presentedBuilding.getInvertedXInstance();
            }

            stringBuilder.append("[" + presentedBuilding.getX() + "," + presentedBuilding.getY() + "] ");
            stringBuilder.append(presentedBuilding.getPlayerType() +"|");
            stringBuilder.append(presentedBuilding.getConstructionTimeLeft() +"|");
            stringBuilder.append(presentedBuilding.getHealth() +"|");
            stringBuilder.append(presentedBuilding.getWeaponCooldownTimeLeft() +"|");
            stringBuilder.append(presentedBuilding.getWeaponDamage() +"|");
            stringBuilder.append(presentedBuilding.getEnergyGeneratedPerTurn() +"\n");
        }
        stringBuilder.append("###############################\n");
        stringBuilder.append('\n');

        stringBuilder.append("####### MISSILE DATA ########\n");

        stringBuilder.append("FORMAT : [x,y] Owner|Damage \n\n");

        for (Missile missile :
                tdMap.getMissiles()) {

            Missile presentedMissile = missile;
            if (playerType == PlayerType.B) {
                presentedMissile = presentedMissile.getInvertedXInstance();
            }

            stringBuilder.append("[" + presentedMissile.getX() + "," + presentedMissile.getY() + "] ");
            stringBuilder.append(presentedMissile.getPlayerType() + "|");
            stringBuilder.append(presentedMissile.getDamage() + "\n");
        }
        stringBuilder.append("###############################\n");

        return stringBuilder.toString();
    }

    private String getRowStringForPlayer(CellStateContainer[] row, int y){
        StringBuilder stringBuilderRow = new StringBuilder();

        for (int x  = 0; x < row.length; x ++) {
            stringBuilderRow.append("[");
            stringBuilderRow.append(x + "," + y + ",");

            if (row[x].buildings.size() > 0){
                Building building = row[x].buildings.get(0);
                if (building.isConstructed()){
                    stringBuilderRow.append(building.getIcon().toUpperCase());
                }else{
                    stringBuilderRow.append(building.getIcon().toLowerCase());
                }
            }else{
                stringBuilderRow.append("N");
            }

            stringBuilderRow.append(",");
            long playerAMissileCount = row[x].missiles.stream()
                    .filter(p -> p.getPlayerType() == PlayerType.A)
                    .count();
            long playerBMissileCount = row[x].missiles.stream()
                    .filter(p -> p.getPlayerType() == PlayerType.B)
                    .count();

            stringBuilderRow.append(playerAMissileCount + "," + playerBMissileCount);
            stringBuilderRow.append("]");
        }

        return stringBuilderRow.toString();
    }

    @Override
    public String commandPrompt(GamePlayer gamePlayer) {
        return "";
    }

}
