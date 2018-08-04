package za.co.entelect.challenge.core.engine;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.commands.DeconstructBuildingCommand;
import za.co.entelect.challenge.commands.DoNothingCommand;
import za.co.entelect.challenge.commands.IronCurtainCommand;
import za.co.entelect.challenge.commands.PlaceBuildingCommand;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.command.RawCommand;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.game.GameRoundProcessor;
import za.co.entelect.challenge.game.contracts.map.GameMap;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class TowerDefenseRoundProcessor implements GameRoundProcessor {

    private static final Logger log = LogManager.getLogger(TowerDefenseRoundProcessor.class);

    private TowerDefenseGameMap towerDefenseGameMap;

    @Override
    public boolean processRound(GameMap gameMap, Hashtable<GamePlayer, RawCommand> commands) {
        towerDefenseGameMap = (TowerDefenseGameMap) gameMap;
        towerDefenseGameMap.clearErrorList();
        towerDefenseGameMap.clearTeslaTargetList();
        towerDefenseGameMap.clearIroncurtainHitList();

        processCommands(commands);

        constructBuildings();

        updateIronCurtains();
        fireTeslaTowers();

        createMissilesFromAttackBuildings();
        moveMissiles();

        removeDeadEntities();
        addResources();

        return true;
    }

    @Override
    public ArrayList<String> getErrorList() {
        return towerDefenseGameMap.getErrorList();
    }

    private void constructBuildings() {
        towerDefenseGameMap.getBuildings()
                .forEach(b -> b.decreaseConstructionTimeLeft());
    }

    private void updateIronCurtains() {
        towerDefenseGameMap.getTowerDefensePlayers()
                .forEach(p -> p.updateIronCurtain(towerDefenseGameMap.getCurrentRound()));
    }

    private void createMissilesFromAttackBuildings() {
        towerDefenseGameMap.getBuildings().stream()
                .filter(b -> b.isConstructed() && b.getBuildingType() == BuildingType.ATTACK)
                .forEach(b -> towerDefenseGameMap.addMissileFromBuilding(b));
    }

    private void fireTeslaTowers() {
        List<Building> playerTeslaTowers = towerDefenseGameMap.getBuildings().stream()
                .filter(b -> b.isConstructed() && b.getBuildingType() == BuildingType.TESLA)
                .sorted(Comparator.comparing(b -> b.getConstructionTimeLeft()))
                .collect(Collectors.toList());

        for (Building teslaTower : playerTeslaTowers) {

            TowerDefensePlayer currentPlayer = null;
            try {
                currentPlayer = towerDefenseGameMap.getPlayer(teslaTower.getPlayerType());
            } catch (Exception e) {
                log.error(e);
            }

            int playerEnergy = currentPlayer.getEnergy();

            if (playerEnergy >= teslaTower.getEnergyPerShot() && teslaTower.getWeaponCooldownTimeLeft() == 0) {
                try {
                    currentPlayer.removeEnergy(teslaTower.getEnergyPerShot());
                } catch (Exception e) {
                    log.error(e);
                }

                towerDefenseGameMap.fireTeslaTower(teslaTower);
                teslaTower.resetCooldown();
            } else {

                if (teslaTower.getWeaponCooldownTimeLeft() > 0) {
                    teslaTower.decreaseCooldown();
                }
            }
        }
    }

    private void processCommands(Hashtable<GamePlayer, RawCommand> commands) {
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
                    int energy = GameConfig.getRoundIncomeEnergy()
                            + getBuildingGeneratedEnergyForPlayer(p.getPlayerType());
                    try {
                        p.addEnergy(energy);
                    } catch (Exception e) {
                        log.error(e);
                    }
                    p.addScore(energy * GameConfig.getEnergyScoreMultiplier());
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

    private void moveMissiles() {
        int maxMissileSpeed = towerDefenseGameMap.getMissiles().stream()
                .mapToInt(m -> m.getSpeed())
                .max().orElse(0);

        IntStream.rangeClosed(1, maxMissileSpeed)
                .forEach(i -> new ArrayList<>(towerDefenseGameMap.getMissiles()).stream()
                        .filter(m -> m.getUnprocessedMovement() > 0)
                        .forEach(missile -> towerDefenseGameMap.moveMissileSingleSpace(missile))
                );

        towerDefenseGameMap.getMissiles()
                .forEach(m -> m.resetUnprocessedMovement());
    }

    // Converts Raw Commands into Commands the game engine can understand
    private void parseAndExecuteCommand(RawCommand command, GamePlayer player) {
        String commandSting = command.getCommand();
        String[] commandLine = commandSting.split(",");

        DoNothingCommand doNothingCommand = new DoNothingCommand();
        TowerDefensePlayer towerDefensePlayer = (TowerDefensePlayer) player;

        if (commandLine.length == 1) {
            doNothingCommand.performCommand(towerDefenseGameMap, player, commandLine[0].equals("Exception"));
            return;
        }
        if (commandLine.length != 3) {
            doNothingCommand.performCommand(towerDefenseGameMap, player, true);
            towerDefenseGameMap.addErrorToErrorList(String.format(
                    "Unable to parse command expected 3 parameters, got %d", commandLine.length), towerDefensePlayer);
        }

        try {
            int positionX = Integer.parseInt(commandLine[0]);
            int positionY = Integer.parseInt(commandLine[1]);
            BuildingType buildingType = BuildingType.values()[Integer.parseInt(commandLine[2])];

            towerDefensePlayer.setConsecutiveDoNothings(0);
            if (buildingType == BuildingType.DECONSTRUCT) {
                new DeconstructBuildingCommand(positionX, positionY).performCommand(towerDefenseGameMap, player);
            } else if (buildingType == BuildingType.IRONCURTAIN) {
                new IronCurtainCommand().performCommand(towerDefenseGameMap, player);
            } else {
                new PlaceBuildingCommand(positionX, positionY, buildingType).performCommand(towerDefenseGameMap, player);
            }
        } catch (NumberFormatException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player, true);
            towerDefenseGameMap.addErrorToErrorList(String.format(
                    "Unable to parse command entries, all parameters should be integers. Received:%s",
                    commandSting), towerDefensePlayer);

            log.error(towerDefenseGameMap.getErrorList().get(towerDefenseGameMap.getErrorList().size() - 1));
        } catch (IllegalArgumentException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player, true);
            towerDefenseGameMap.addErrorToErrorList(String.format(
                    "Unable to parse building type: Expected 0[Defense], 1[Attack], 2[Energy], 3[Deconstruct], 4[Tesla], 5[Iron Curtain]. Received:%s",
                    commandLine[2]), towerDefensePlayer);

            log.error(towerDefenseGameMap.getErrorList().get(towerDefenseGameMap.getErrorList().size() - 1));
        } catch (InvalidCommandException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player, true);
            towerDefenseGameMap.addErrorToErrorList(
                    "Invalid command received: " + e.getMessage(), towerDefensePlayer);

            log.error(towerDefenseGameMap.getErrorList().get(towerDefenseGameMap.getErrorList().size() - 1));
        } catch (IndexOutOfBoundsException e) {
            doNothingCommand.performCommand(towerDefenseGameMap, player, true);
            towerDefenseGameMap.addErrorToErrorList(String.format(
                    "Out of map bounds, X:%s Y: %s", commandLine[0], commandLine[1]), towerDefensePlayer);

            log.error(towerDefenseGameMap.getErrorList().get(towerDefenseGameMap.getErrorList().size() - 1));
        }
    }
}
