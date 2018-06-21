package za.co.entelect.challenge.core.engine;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.game.contracts.exceptions.TimeoutException;
import za.co.entelect.challenge.game.contracts.game.GameEngine;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.List;

public class TowerDefenseGameEngine implements GameEngine {

    public TowerDefenseGameEngine(String configLocation) {
        GameConfig.initConfig(configLocation);
    }

    @Override
    public boolean isGameComplete(GameMap gameMap) throws TimeoutException {
        TowerDefenseGameMap towerDefenseGameMap = (TowerDefenseGameMap) gameMap;

        if (!playersInValidState(towerDefenseGameMap)) {
            throw new TimeoutException("Too many do nothing commands received due to exceptions");
        }

        return gameMap.getCurrentRound() > GameConfig.getMaxRounds() || (towerDefenseGameMap.getDeadPlayers().size() > 0);
    }

    private boolean playersInValidState(TowerDefenseGameMap towerDefenseGameMap) {
        List<TowerDefensePlayer> towerDefensePlayers = towerDefenseGameMap.getTowerDefensePlayers();
        for (TowerDefensePlayer towerDefensePlayer : towerDefensePlayers) {
            if (towerDefensePlayer.getConsecutiveDoNothings() >= GameConfig.getMaxDoNothings()) {
                return false;
            }
        }

        return true;
    }
}
