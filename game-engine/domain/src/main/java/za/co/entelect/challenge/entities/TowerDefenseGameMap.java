package za.co.entelect.challenge.entities;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.enums.Direction;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TowerDefenseGameMap implements GameMap {

    private List<TowerDefensePlayer> towerDefensePlayers = new ArrayList<>();
    private ArrayList<Building> buildings = new ArrayList<>();
    private ArrayList<Missile> missiles = new ArrayList<>();
    private ArrayList<String> errorList = new ArrayList<>();
    private int currentRound;

    public List<TowerDefensePlayer> getTowerDefensePlayers() {
        return towerDefensePlayers;
    }

    public List<Building> getBuildings() {
        return buildings;
    }

    public ArrayList<String> getErrorList() {
        return this.errorList;
    }

    public void clearErrorList() {
        this.errorList = new ArrayList<>();
    }

    public void addErrorToErrorList(String error, TowerDefensePlayer player) {
        this.errorList.add(String.format("Player %s: %s", player.getPlayerType(), error));
    }

    public void addErrorToErrorList(String error, PlayerType playerType) {
        this.errorList.add(String.format("Player %s: %s", playerType, error));
    }

    public List<Missile> getMissiles() {
        return missiles;
    }

    public void registerPlayer(TowerDefensePlayer towerDefensePlayer) {
        towerDefensePlayers.add(towerDefensePlayer);
    }

    public Stream<TowerDefensePlayer> getPlayerByStream(PlayerType id) {
        return towerDefensePlayers.stream()
                .filter(p -> p.getPlayerType() == id);
    }

    public TowerDefensePlayer getPlayer(PlayerType id) throws Exception {
        Optional<TowerDefensePlayer> potentialPlayer = towerDefensePlayers.stream()
                .filter((player -> player.getPlayerType() == id))
                .findFirst();
        if (!potentialPlayer.isPresent()) {
            throw new Exception(String.format("No player %s registered", id));
        }
        return potentialPlayer.get();
    }

    public TowerDefensePlayer getPlayerOpponent(PlayerType id) throws Exception {
        Optional<TowerDefensePlayer> potentialPlayer = towerDefensePlayers.stream()
                .filter((player -> player.getPlayerType() != id))
                .findFirst();
        if (!potentialPlayer.isPresent()) {
            throw new Exception(String.format("No opponent to %s registered", id));
        }
        return potentialPlayer.get();
    }

    public void addBuilding(Building building) {
        buildings.add(building);
        try {
            getPlayer(building.getPlayerType()).addScore(building.getConstructionScore());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void removeBuilding(Building building) {
        buildings.remove(building);
        towerDefensePlayers.stream()
                .filter(p -> !building.isPlayers(p.getPlayerType()))
                .forEach(p -> p.addScore(building.getDestroyMultiplier()));
    }

    public void addMissileFromBuilding(Building b) {
        if (b.getWeaponDamage() == 0) {
            return;
        }

        if (b.getWeaponCooldownTimeLeft() > 0) {
            b.decreaseCooldown();
            return;
        }

        Direction direction = null;
        if (b.isPlayers(PlayerType.A)) {
            direction = Direction.RIGHT;
        } else if (b.isPlayers(PlayerType.B)) {
            direction = Direction.LEFT;
        }

        missiles.add(new Missile(b, direction));
        b.resetCooldown();
    }

    public void removeMissile(Missile missile) {
        this.missiles.remove(missile);
    }

    public void moveMissileSingleSpace(Missile p) throws Exception {
        int newPosition = p.getX() + (p.getDirection().getMultiplier());

        boolean homeBaseIsHit = (newPosition < 0 || newPosition >= GameConfig.getMapWidth());
        if (homeBaseIsHit) {
            TowerDefensePlayer opponent = getPlayerOpponent(p.playerType);
            TowerDefensePlayer missileOwner = getPlayer(p.playerType);
            opponent.takesHitByPlayer(p, missileOwner);

            p.setSpeed(0);
        }

        int offsetToMove = p.getDirection().getMultiplier();
        p.moveX(offsetToMove);
    }

    @Override
    public int getCurrentRound() {
        return currentRound;
    }

    @Override
    public void setCurrentRound(int currentRound) {
        this.currentRound = currentRound;
    }

    public List<GamePlayer> getDeadPlayers() {
        return getTowerDefensePlayers().stream()
                .filter(p -> p.getHealth() <= 0)
                .collect(Collectors.toList());
    }

    @Override
    public GamePlayer getWinningPlayer() {
        List<GamePlayer> deadPlayers = getDeadPlayers();
        TowerDefensePlayer winner = null;
        if (deadPlayers.size() == 1) {
            try {
                winner = getPlayerOpponent(((TowerDefensePlayer) deadPlayers.get(0)).getPlayerType());
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            for (GamePlayer gamePlayer :
                    getTowerDefensePlayers()) {
                TowerDefensePlayer tdPlayer = (TowerDefensePlayer) gamePlayer;
                if (winner == null) {
                    winner = tdPlayer;
                } else {
                    if (winner.getScore() == tdPlayer.getScore()) {
                        return null;
                    }
                    if (winner.getScore() < tdPlayer.getScore()) {
                        winner = tdPlayer;
                    }
                }
            }
        }

        return winner;
    }
}
