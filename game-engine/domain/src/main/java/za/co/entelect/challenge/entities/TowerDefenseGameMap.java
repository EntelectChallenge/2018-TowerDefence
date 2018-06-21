package za.co.entelect.challenge.entities;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.commands.PlaceBuildingCommand;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.Direction;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TowerDefenseGameMap implements GameMap {

    private List<TowerDefensePlayer> towerDefensePlayers = new ArrayList<>();
    private ArrayList<Building> buildings = new ArrayList<>();
    private ArrayList<Missile> missiles = new ArrayList<>();
    private ArrayList<String> errorList = new ArrayList<>();
    private int currentRound;

    public TowerDefenseGameMap() {
        buildings.add(new Building(0, 4, PlayerType.A, 5, 1, 20, 20, 5, 5, "T", 0, 5, 0, 9,100, BuildingType.TESLA));
        buildings.add(new Building(8, 4, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
        buildings.add(new Building(9, 3, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
        buildings.add(new Building(10, 4, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
        buildings.add(new Building(11, 4, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
        buildings.add(new Building(12, 5, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
        buildings.add(new Building(13, 3, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
        buildings.add(new Building(14, 3, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
        buildings.add(new Building(15, 3, PlayerType.B, 5, 1, 20, 0, 5, 5, "D", 0, 5, 0, 0, BuildingType.DEFENSE));
    }

    private static final Logger log = LogManager.getLogger(PlaceBuildingCommand.class);

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

    private TowerDefensePlayer getPlayerOpponent(PlayerType id) throws Exception {
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
            log.error(e);
        }
    }

    public void removeBuilding(Building building) {
        buildings.remove(building);
        try {
            getPlayerOpponent(building.getPlayerType()).addScore(building.getDestroyMultiplier());
        } catch (Exception e) {
            log.error(e);
        }
    }

    private Direction checkAndSetupMissiles(Building b) {
        if (b.getWeaponDamage() == 0) {
            return null;
        }

        if (b.getWeaponCooldownTimeLeft() > 0) {
            b.decreaseCooldown();
            return null;
        }

        Direction direction = null;
        if (b.isPlayers(PlayerType.A)) {
            direction = Direction.RIGHT;
        } else if (b.isPlayers(PlayerType.B)) {
            direction = Direction.LEFT;
        }

        return direction;
    }

    public void fireTeslaTower(Building teslaBuilding) {
        Direction direction = checkAndSetupMissiles(teslaBuilding);

        if (direction == null)
            return;

        ArrayList<Building> possibleTargets = new ArrayList<>();

        buildings.stream()
                .filter(building -> building.isConstructed()
                        && (
                        building.getY() >= (teslaBuilding.getY() - 1)
                                && building.getY() <= (teslaBuilding.getY() + 1)

                )
                        && !building.isPlayers(teslaBuilding.getPlayerType())
                        && building.getHealth() > 0)
                .forEach(enemyBuilding -> {
                    possibleTargets.add(enemyBuilding);
                });

        TowerDefensePlayer missileOwner = null;
        try {
            missileOwner = getPlayer(teslaBuilding.getPlayerType());
        } catch (Exception e) {
            log.error(e);
        }

        //Direction in the left  hand as well.
        for (int x = teslaBuilding.getX() + 1; x <= teslaBuilding.getMaxRange() + teslaBuilding.getX(); x++) {
            final int nextTargetpoint = x;
            Building targetToHit;

            ArrayList<Building> targetsInX = new ArrayList<>();

            possibleTargets.stream()
                    .filter(target -> target.getX() == nextTargetpoint)
                    .forEach(target -> {
                        targetsInX.add(target);
                    });

            targetsInX.sort(Comparator.comparing(Building::getY));

            if (targetsInX.size() > 0) {
                targetToHit = targetsInX.get(0);

                if (targetToHit != null) {
                    targetToHit.damageSelfDirectly(teslaBuilding.getWeaponDamage(), missileOwner);
                    possibleTargets.remove(targetToHit);
                }
            }
        }
    }

    public void addMissileFromBuilding(Building b) {
        Direction direction = checkAndSetupMissiles(b);

        missiles.add(new Missile(b, direction));
        b.resetCooldown();
    }

    public void removeMissile(Missile missile) {
        this.missiles.remove(missile);
    }

    private static boolean positionMatch(Cell a, Cell b) {
        return (a.getY() == b.getY()) && (a.getX() == b.getX());
    }

    public void moveMissileSingleSpace(Missile p) {
        int offsetToMove = p.getDirection().getMultiplier();
        p.moveX(offsetToMove);
        p.reduceUnprocessedMovement();

        checkHomeBaseHit(p);
        checkBuildingsHit(p);
    }

    private void checkBuildingsHit(Missile p) {
        buildings.stream()
                .filter(b -> b.isConstructed()
                        && positionMatch(p, b)
                        && !b.isPlayers(p.getPlayerType())
                        && b.getHealth() > 0)
                .forEach(b -> {
                    try {
                        TowerDefensePlayer missileOwner = getPlayer(p.getPlayerType());
                        b.damageSelf(p, missileOwner);
                    } catch (Exception e) {
                        log.error(e);
                    }
                    removeMissile(p);
                });
    }

    private void checkHomeBaseHit(Missile p) {
        boolean homeBaseIsHit = (p.getX() < 0 || p.getX() >= GameConfig.getMapWidth());
        if (homeBaseIsHit) {

            TowerDefensePlayer opponent = null;
            try {
                opponent = getPlayerOpponent(p.playerType);
            } catch (Exception e) {
                log.error(e);
            }

            TowerDefensePlayer missileOwner = null;
            try {
                missileOwner = getPlayer(p.playerType);
            } catch (Exception e) {
                log.error(e);
            }

            if (opponent != null) {
                opponent.takesHitByPlayer(p, missileOwner);
            }

            removeMissile(p);
        }
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
        List<TowerDefensePlayer> players = getTowerDefensePlayers();
        TowerDefensePlayer winner = null;

        if (deadPlayers.size() == 1) {
            try {
                winner = getPlayerOpponent(((TowerDefensePlayer) deadPlayers.get(0)).getPlayerType());
            } catch (Exception e) {
                log.error(e);
            }
        } else if (deadPlayers.size() == 0 || deadPlayers.size() == 2) {
            TowerDefensePlayer playerA = players.get(0);
            TowerDefensePlayer playerB = players.get(1);

            if (playerA.getScore() != playerB.getScore()) {
                winner = players.stream()
                        .max(Comparator.comparingInt(TowerDefensePlayer::getScore))
                        .get();
            }
        }

        return winner; // If winner is null, game ended in a tie
    }
}
