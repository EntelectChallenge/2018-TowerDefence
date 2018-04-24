package za.co.entelect.challenge.core.engine;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.game.GameMapGenerator;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.player.Player;

import java.util.List;

public class TowerDefenseGameMapGenerator implements GameMapGenerator {

    @Override
    public GameMap generateGameMap(List<Player> players) {

        TowerDefenseGameMap towerDefenseGameMap = new TowerDefenseGameMap();

        TowerDefensePlayer towerDefensePlayer1 = new TowerDefensePlayer(PlayerType.A, GameConfig.getStartEnergy(), GameConfig.getStartingHealth());
        players.get(0).setGamePlayer(towerDefensePlayer1);
        TowerDefensePlayer towerDefensePlayer2 = new TowerDefensePlayer(PlayerType.B, GameConfig.getStartEnergy(), GameConfig.getStartingHealth());
        players.get(1).setGamePlayer(towerDefensePlayer2);

        towerDefenseGameMap.registerPlayer(towerDefensePlayer1);
        towerDefenseGameMap.registerPlayer(towerDefensePlayer2);

        return towerDefenseGameMap;
    }
}
