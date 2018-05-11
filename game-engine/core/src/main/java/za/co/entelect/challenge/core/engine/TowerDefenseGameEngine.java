package za.co.entelect.challenge.core.engine;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.game.contracts.game.GameEngine;
import za.co.entelect.challenge.game.contracts.map.GameMap;

public class TowerDefenseGameEngine implements GameEngine {

    public TowerDefenseGameEngine(String configLocation) {
        GameConfig.initConfig(configLocation);
    }

    @Override
    public boolean isGameComplete(GameMap gameMap) {
        TowerDefenseGameMap towerDefenseGameMap = (TowerDefenseGameMap) gameMap;
        if (gameMap.getCurrentRound() > GameConfig.getMaxRounds()) {
            return true;
        }

        return (towerDefenseGameMap.getDeadPlayers().size() > 0);
    }

}
