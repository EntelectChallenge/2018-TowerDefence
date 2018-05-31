package za.co.entelect.challenge.core.renderers;

import com.google.gson.Gson;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.core.entities.PlayerData;
import za.co.entelect.challenge.core.entities.TowerDefenseJsonContainer;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;

import java.util.ArrayList;

public class TowerDefenseJsonGameMapRenderer implements GameMapRenderer {

    private TowerDefenseGameMap towerDefenseGameMap;

    private static final Logger log = LogManager.getLogger(TowerDefenseJsonGameMapRenderer.class);

    @Override
    public String render(GameMap gameMap, GamePlayer player) {
        if(gameMap instanceof TowerDefenseGameMap) {
            towerDefenseGameMap = (TowerDefenseGameMap) gameMap;
            TowerDefensePlayer towerDefensePlayer = (TowerDefensePlayer) player;

            Gson gson = new Gson();
            TowerDefenseJsonContainer container;
            if (towerDefensePlayer.getPlayerType() == PlayerType.A){
                container = new TowerDefenseJsonContainer(RendererHelper.renderPlayerA(towerDefenseGameMap), getPlayerDataForPlayer(player), towerDefenseGameMap.getCurrentRound());
            }else{
                container = new TowerDefenseJsonContainer(RendererHelper.renderPlayerB(towerDefenseGameMap), getPlayerDataForPlayer(player), towerDefenseGameMap.getCurrentRound());
            }
            if (container == null){
               log.error("The container cannot be empty.");
                return "";
            }
            return gson.toJson(container);
        }

        log.error("The gamemmap must be an instance of the tower defence game map");
        return "";
    }

    private PlayerType invertPlayerType(PlayerType type){
        return type == PlayerType.A ? PlayerType.B : PlayerType.A;
    }

    public PlayerData[] getPlayerDataForPlayer(GamePlayer player){
        ArrayList<PlayerData> playerDatum = new ArrayList<>();
        for (TowerDefensePlayer tdPlayer : towerDefenseGameMap.getTowerDefensePlayers()) {
            PlayerType playerType = tdPlayer.getPlayerType();
            //if you are getting data for player B, then invert the player type for rendering.
            if (((TowerDefensePlayer) player).getPlayerType() == PlayerType.B) {
                playerType = invertPlayerType(tdPlayer.getPlayerType());
            }

            PlayerData playerData = new PlayerData(playerType, tdPlayer.getEnergy(), tdPlayer.getHealth(), tdPlayer.getHitsTaken(), tdPlayer.getScore());
            playerDatum.add(playerData);
        }
        return playerDatum.toArray(new PlayerData[0]);
    }

    @Override
    public String commandPrompt(GamePlayer gamePlayer) {
        return "";
    }
}
