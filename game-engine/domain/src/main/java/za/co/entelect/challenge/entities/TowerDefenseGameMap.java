package za.co.entelect.challenge.entities;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.commands.PlaceBuildingCommand;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.enums.Direction;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TowerDefenseGameMap implements GameMap {

    private List<TowerDefensePlayer> towerDefensePlayers = new ArrayList<>();
    private ArrayList<Building> buildings = new ArrayList<>();
    private ArrayList<Missile> missiles = new ArrayList<>();
    private ArrayList<String> errorList = new ArrayList<>();
    private ArrayList<List<Cell>> teslaTargetList = new ArrayList<>();
    private List<Cell> ironcurtainHitList = new ArrayList<>();
    private int currentRound;

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

    public void clearTeslaTargetList() {
        this.teslaTargetList = new ArrayList<>();
    }

    public void clearIroncurtainHitList() {
        this.ironcurtainHitList = new ArrayList<>();
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

    public void deconstructBuilding(Building building) {
        buildings.remove(building);
    }

    public void removeBuilding(Building building) {
        buildings.remove(building);
    }

    private Direction getDirectionAndUpdateBuilding(Building b) {
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

    private Direction getDirectionToFireAt(Building b) {
        if (b.getWeaponDamage() == 0) {
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
        Direction direction = getDirectionToFireAt(teslaBuilding);
        if (direction == null) {
            return;
        }

        List<Building> allTargets = buildings.stream()
                .filter(b -> b.isConstructed()
                        && (b.getY() >= (teslaBuilding.getY() - 1)
                        && b.getY() <= (teslaBuilding.getY() + 1))
                        && !b.isPlayers(teslaBuilding.getPlayerType())
                        && b.getHealth() > 0)
                .collect(Collectors.toList());

        TowerDefensePlayer opponentPlayer = null;
        TowerDefensePlayer missileOwner = null;
        try {
            missileOwner = getPlayer(teslaBuilding.getPlayerType());
            opponentPlayer = getPlayerOpponent(teslaBuilding.getPlayerType());
        } catch (Exception e) {
            log.error(e);
        }

        List<Cell> targetHits = new ArrayList<>();
        if (direction.equals(Direction.RIGHT)) {

            if (opponentPlayer.isIroncurtainActive()) {
                addTeslaHitListMarkers(teslaBuilding, targetHits, GameConfig.getMapWidth() / 2);
                return; // Tesla tower cannot affect anything if the Iron Curtain is active
            }

            if (teslaBuilding.getX() == (GameConfig.getMapWidth() / 2) - 1) {
                targetHits.add(new Cell(opponentPlayer.getPlayerType()));
                opponentPlayer.takesHitByPlayer(teslaBuilding.getWeaponDamage(), missileOwner);
            }

            for (int columnToFireAt = teslaBuilding.getX() + 1;
                 columnToFireAt <= teslaBuilding.getMaxRange() + teslaBuilding.getX();
                 columnToFireAt++) {
                targetHits.addAll(teslaFireAtBuildingInColumn(columnToFireAt, allTargets, teslaBuilding, missileOwner));
            }
        } else if (direction.equals(Direction.LEFT)) {

            if (opponentPlayer.isIroncurtainActive()) {
                addTeslaHitListMarkers(teslaBuilding, targetHits, (GameConfig.getMapWidth() / 2) - 1);
                return; // Tesla tower cannot affect anything if the Iron Curtain is active
            }

            if (teslaBuilding.getX() == (GameConfig.getMapWidth() / 2)) {
                targetHits.add(new Cell(opponentPlayer.getPlayerType()));
                opponentPlayer.takesHitByPlayer(teslaBuilding.getWeaponDamage(), missileOwner);
            }

            for (int x = teslaBuilding.getX() - 1; x >= teslaBuilding.getX() - teslaBuilding.getMaxRange(); x--) {
                targetHits.addAll(teslaFireAtBuildingInColumn(x, allTargets, teslaBuilding, missileOwner));
            }
        }

        if (targetHits.size() > 0) {
            targetHits.add(new Cell(teslaBuilding.getX(), teslaBuilding.getY(), teslaBuilding.getPlayerType()));
            this.teslaTargetList.add(targetHits);
        }
    }

    private void addTeslaHitListMarkers(Building teslaBuilding, List<Cell> targetHits, int shieldX) {
        this.ironcurtainHitList.add(new Cell(shieldX, teslaBuilding.getY(), teslaBuilding.getPlayerType()));
        targetHits.add(new Cell(teslaBuilding.getX(), teslaBuilding.getY(), teslaBuilding.getPlayerType()));
        targetHits.add(new Cell(shieldX, teslaBuilding.getY(), teslaBuilding.getPlayerType()));
        this.teslaTargetList.add(targetHits);
    }

    private List<Cell> teslaFireAtBuildingInColumn(int columnToFireAt,
                                                   List<Building> possibleTargets,
                                                   Building teslaBuilding,
                                                   TowerDefensePlayer missileOwner) {
        List<Cell> teslaHitBuildings = new ArrayList<>();
        final int nextTargetPoint = columnToFireAt;
        List<Building> targetsInX = possibleTargets.stream()
                .filter(target -> target.getX() == nextTargetPoint)
                .sorted(Comparator.comparing(b -> b.getY()))
                .collect(Collectors.toList());

        if (targetsInX.size() > 0) {
            Building targetToHit = targetsInX.get(0);

            if (targetToHit != null) {
                targetToHit.damageSelfDirectly(teslaBuilding.getWeaponDamage(), missileOwner);
                teslaHitBuildings.add(new Cell(targetToHit.getX(), targetToHit.getY(), targetToHit.getPlayerType()));
            }
        }

        return teslaHitBuildings;
    }

    private boolean columnIsIroncurtained(int columnToCheck, PlayerType missileOwnerId, boolean singleColumn) {
        boolean opponentCurtainIsActive = false;
        try {
            opponentCurtainIsActive = getPlayerOpponent(missileOwnerId).isIroncurtainActive();
        } catch (Exception e) {
            log.error(e);
        }

        if (!opponentCurtainIsActive) {
            return false;
        }

        int halfMapWidth = GameConfig.getMapWidth() / 2;
        if (missileOwnerId == PlayerType.A) {
            int ironcurtainColumn = halfMapWidth; // Player B is shielded
            if (columnToCheck == ironcurtainColumn) {
                return true;
            } else if (!singleColumn && columnToCheck >= ironcurtainColumn) {
                return true;
            }

        } else if (missileOwnerId == PlayerType.B) {
            int ironcurtainColumn = halfMapWidth - 1; // Player A is shielded
            if (columnToCheck == ironcurtainColumn) {
                return true;
            } else if (!singleColumn && columnToCheck <= ironcurtainColumn) {
                return true;
            }
        }

        return false;
    }

    public boolean shieldExists() {
        return getTowerDefensePlayers().stream()
                .anyMatch(p -> p.isIroncurtainActive());
    }

    public void addMissileFromBuilding(Building b) {
        Direction direction = getDirectionAndUpdateBuilding(b);

        if (direction == null) {
            return;
        }

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

        // Missiles already past the shield column are NOT stopped
        if (columnIsIroncurtained(p.getX(), p.playerType, true)) {
            removeMissile(p);
            this.ironcurtainHitList.add(new Cell(p.getX(), p.getY(), p.getPlayerType()));
            return;
        }

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

    public ArrayList<List<Cell>> getTeslaTargetList() {
        return teslaTargetList;
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

    public void activateIronCurtain(TowerDefensePlayer player) {
        player.activateIronCurtain();
    }

    public List<Cell> getIroncurtainHitList() {
        return ironcurtainHitList;
    }
}
