package za.co.entelect.challenge.core.renderers;

import com.google.gson.Gson;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.core.entities.CellStateContainer;
import za.co.entelect.challenge.core.entities.PlayerData;
import za.co.entelect.challenge.core.entities.TowerDefenseJsonContainer;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;

import java.util.List;
import java.util.stream.Collectors;

public class TowerDefenseJsonGameMapRenderer implements GameMapRenderer {

    private TowerDefenseGameMap towerDefenseGameMap;

    private static final Logger log = LogManager.getLogger(TowerDefenseJsonGameMapRenderer.class);

    @Override
    public String render(GameMap gameMap, GamePlayer player) {
        if (gameMap instanceof TowerDefenseGameMap) {
            towerDefenseGameMap = (TowerDefenseGameMap) gameMap;
            TowerDefensePlayer towerDefensePlayer = (TowerDefensePlayer) player;

            Gson gson = new Gson();
            TowerDefenseJsonContainer container = towerDefensePlayer.getPlayerType() == PlayerType.A
                    ? createTowerDefenseJsonContainer(towerDefensePlayer, RendererHelper.renderPlayerA(towerDefenseGameMap))
                    : createTowerDefenseJsonContainer(towerDefensePlayer, RendererHelper.renderPlayerB(towerDefenseGameMap));

            return gson.toJson(container);
        }

        log.error("The gamemmap must be an instance of the tower defence game map");
        return "";
    }

    private TowerDefenseJsonContainer createTowerDefenseJsonContainer(TowerDefensePlayer towerDefensePlayer, CellStateContainer[][] cellStateContainers) {
        return new TowerDefenseJsonContainer(
                cellStateContainers,
                getPlayersDataForPlayer(towerDefensePlayer),
                towerDefenseGameMap.getCurrentRound(),
                towerDefenseGameMap.getTeslaTargetList(),
                towerDefenseGameMap.getIroncurtainHitList()
        );
    }

    private PlayerType invertPlayerType(PlayerType type) {
        return type == PlayerType.A ? PlayerType.B : PlayerType.A;
    }

    public List<PlayerData> getPlayersDataForPlayer(TowerDefensePlayer towerDefensePlayer) {
        return towerDefenseGameMap.getTowerDefensePlayers().stream()
                .map(p -> {
                    PlayerType playerType = p.getPlayerType();
                    //if you are getting data for player B, then invert the player type for rendering.
                    if (towerDefensePlayer.getPlayerType() == PlayerType.B) {
                        playerType = invertPlayerType(p.getPlayerType());
                    }

                    return new PlayerData(
                            playerType,
                            p.getEnergy(),
                            p.getHealth(),
                            p.getHitsTaken(),
                            p.getScore(),
                            p.canPlaceIronCurtain(),
                            p.getActiveIronCurtainTimeLeft(),
                            p.isIroncurtainActive()
                    );
                })
                .collect(Collectors.toList());
    }

    @Override
    public String commandPrompt(GamePlayer gamePlayer) {
        return "";
    }
}
