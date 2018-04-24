package za.co.entelect.challenge.core.engine;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.core.renderers.TowerDefenseTextMapRenderer;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.game.contracts.game.GameEngine;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class TowerDefenseGameEngine implements GameEngine {

    @Override
    public boolean isGameComplete(GameMap gameMap) {
        TowerDefenseGameMap towerDefenseGameMap = (TowerDefenseGameMap) gameMap;
        if (gameMap.getCurrentRound() > GameConfig.getMaxRounds()) {
            addPlayersHealthAsScore(gameMap);
            return true;
        }

        if (towerDefenseGameMap.getDeadPlayers().size() > 0) {
            addPlayersHealthAsScore(gameMap);
            return true;
        }
        return false;
    }


    private void addPlayersHealthAsScore(GameMap gameMap) {
        ((TowerDefenseGameMap) gameMap).getTowerDefensePlayers()
                .forEach(p -> p.addScore(p.getHealth() * GameConfig.getHealthScoreMultiplier()));
    }
}
