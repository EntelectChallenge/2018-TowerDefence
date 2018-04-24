package za.co.entelect.challenge.core.engine;

import za.co.entelect.challenge.commands.DoNothingCommand;
import za.co.entelect.challenge.commands.PlaceBuildingCommand;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.Cell;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.game.GameRoundProcessor;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Map;
import java.util.stream.IntStream;

public class TowerDefenseRoundProcessor implements GameRoundProcessor {

    private TowerDefenseGameMap towerDefenseGameMap;

    @Override
    public boolean processRound(GameMap gameMap, Hashtable<GamePlayer, RawCommand> commands) {
        towerDefenseGameMap = (TowerDefenseGameMap) gameMap;
        towerDefenseGameMap.clearErrorList();

        try {
            processCommands(commands);

            constructBuildings();

            createMissilesFromGuns();
            calculateMissileMovement();
            removeDeadEntities();

            addResources();

        } catch (Exception e) {
            towerDefenseGameMap.addErrorToErrorList(e.getLocalizedMessage());
            return false;
        }

        return true;
    }

    @Override
    public ArrayList<String> getErrorList() {
        return towerDefenseGameMap.getErrorList();
    }

    private void constructBuildings() {
        towerDefenseGameMap.getBuildings().stream()
                .filter(b -> !b.isConstructed())
                .forEach(Building::decreaseConstructionTimeLeft);
    }

    private void createMissilesFromGuns() {
        towerDefenseGameMap.getBuildings().stream()
                .filter(Building::isConstructed)
                .forEach(b -> towerDefenseGameMap.addMissileFromBuilding(b));
    }

    private void processCommands(Hashtable<GamePlayer, RawCommand> commands) throws InvalidCommandException {
        for (Map.Entry<GamePlayer, RawCommand> gamePlayerCommandEntry : commands.entrySet()) {
            GamePlayer player = gamePlayerCommandEntry.getKey();
            RawCommand command = gamePlayerCommandEntry.getValue();
            parseAndExecuteCommand(command, player);
        }
    }

    private int getBuildingGeneratedEnergyForPlayer(PlayerType player) {
        return towerDefenseGameMap.getBuildings().stream()
                .filter(b -> b.getPlayerType() == player && b.isConstructed())
                .mapToInt(b -> b.getEnergyGeneratedPerTurn())
                .sum();
    }

    private void addResources() {
        towerDefenseGameMap.getTowerDefensePlayers()
                .forEach(p -> {
                    try {
                        int energy = GameConfig.getRoundIncomeEnergy()
                                + getBuildingGeneratedEnergyForPlayer(p.getPlayerType());
                        p.addEnergy(energy);
                        p.addScore(energy * GameConfig.getEnergyScoreMultiplier());
                    } catch (Exception e) {
                        towerDefenseGameMap.addErrorToErrorList(e.getLocalizedMessage());
                    }
                });
    }

    private void removeDeadEntities() {
        new ArrayList<>(towerDefenseGameMap.getBuildings()).stream()
                .filter(b -> b.getHealth() == 0)
                .forEach(b -> towerDefenseGameMap.removeBuilding(b));

        new ArrayList<>(towerDefenseGameMap.getMissiles()).stream()
                .filter(p -> p.getSpeed() == 0)
                .forEach(p -> towerDefenseGameMap.removeMissile(p));
    }

    private static boolean positionMatch(Cell a, Cell b) {
        return (a.getY() == b.getY()) && (a.getX() == b.getX());
    }

    private void calculateMissileMovement() {

        towerDefenseGameMap.getMissiles()
                .forEach(missile -> IntStream.rangeClosed(1, missile.getSpeed()) // higher speed bullets
                        .forEach(i -> {
                            try {
                                towerDefenseGameMap.moveMissileSingleSpace(missile);
                                towerDefenseGameMap.getBuildings().stream()
                                        .filter(b -> b.isConstructed() && positionMatch(missile, b))
                                        .findAny()
                                        .ifPresent(b -> {
                                            b.damageSelf(missile);
                                            towerDefenseGameMap.getPlayerByStream(missile.getPlayerType())
                                                    .forEach(player -> player.addScore(b.getDestroyMultiplier() * missile.getDamage()));
                                        });
                            } catch (Exception e) {
                                towerDefenseGameMap.addErrorToErrorList(e.getLocalizedMessage());
                            }
                        })
                );
    }

    // Converts Raw Commands into Commands the game engine can understand
    private void parseAndExecuteCommand(RawCommand command, GamePlayer player) throws InvalidCommandException {
        String commandSting = command.getCommand();

        String[] commandLine = commandSting.split(",");

        DoNothingCommand doNothingCommand = new DoNothingCommand();

        TowerDefensePlayer towerDefensePlayer = (TowerDefensePlayer) player;

        if (commandLine.length == 1) {
            doNothingCommand.performCommand(towerDefenseGameMap, player);
            return;
        }
        if (commandLine.length != 3) {
            doNothingCommand.performCommand(towerDefenseGameMap, player);
            towerDefenseGameMap.addErrorToErrorList(String.format("Unable to parse command expected 3 parameters, got %d", commandLine.length));
        }
        try {
            int positionX = Integer.parseInt(commandLine[0]);
            int positionY = Integer.parseInt(commandLine[1]);
            BuildingType buildingType = BuildingType.values()[Integer.parseInt(commandLine[2])];

            new PlaceBuildingCommand(positionX, positionY, buildingType).performCommand(towerDefenseGameMap, player);
        } catch (NumberFormatException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player);
            towerDefenseGameMap.addErrorToErrorList(String.format("Unable to parse command entries, all parameters should be integers. RawCommand: %s, for Player: %s", commandSting, towerDefensePlayer.getPlayerType()));
        } catch (IllegalArgumentException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player);
            towerDefenseGameMap.addErrorToErrorList(String.format("Unable to parse building type: Expected 0[Wall] or 1[Turret]. Received:%s, for Player: %s", commandLine[2], towerDefensePlayer.getPlayerType()));
        } catch (InvalidCommandException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player);
            towerDefenseGameMap.addErrorToErrorList("Invalid command received: " + e.getMessage());
        } catch (IndexOutOfBoundsException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player);
            towerDefenseGameMap.addErrorToErrorList(String.format("Out of bounds of the map X:%s Y: %s", commandLine[0], commandLine[1]));
        }
    }


}
