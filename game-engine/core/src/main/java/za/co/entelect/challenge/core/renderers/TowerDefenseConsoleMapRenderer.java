package za.co.entelect.challenge.core.renderers;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.core.entities.ThreeEntityCell;
import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.entities.TowerDefensePlayer;
import za.co.entelect.challenge.enums.Direction;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.game.GamePlayer;
import za.co.entelect.challenge.game.contracts.map.GameMap;
import za.co.entelect.challenge.game.contracts.renderer.GameMapRenderer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

public class TowerDefenseConsoleMapRenderer implements GameMapRenderer {

    public static final String ANSI_RESET = "";
    public static final String ANSI_BLACK = "";
    public static final String ANSI_GRAY = "";
    public static final String ANSI_WHITE = "";
    public static final String ANSI_RED = "";
    public static final String ANSI_GREEN = "";
    public static final String ANSI_BLUE = "";
    public static final String ANSI_YELLOW = "";
    public static final String ANSI_PURPLE = "";
    public static final String ANSI_CYAN = "";
    public static final String ANSI_BLACK_BRIGHT = "";
    public static final String ANSI_RED_BRIGHT = "";
    public static final String ANSI_GREEN_BRIGHT = "";
    public static final String ANSI_YELLOW_BRIGHT = "";
    public static final String ANSI_BLUE_BRIGHT = "";
    public static final String ANSI_PURPLE_BRIGHT = "";
    public static final String ANSI_CYAN_BRIGHT = "";
    public static final String ANSI_WHITE_BRIGHT = "";
    public static final String ANSI_BLACK_BACKGROUND = "";
    public static final String ANSI_WHITE_BACKGROUND = "";
    public static final String ANSI_RED_BACKGROUND = "";
    public static final String ANSI_GREEN_BACKGROUND = "";
    public static final String ANSI_BLUE_BACKGROUND = "";
    public static final String ANSI_YELLOW_BACKGROUND = "";
    public static final String ANSI_PURPLE_BACKGROUND = "";
    public static final String ANSI_CYAN_BACKGROUND = "";

    private static final Logger log = LogManager.getLogger(TowerDefenseConsoleMapRenderer.class);

    @Override
    public String render(GameMap gameMap, GamePlayer player) {
        if (gameMap instanceof TowerDefenseGameMap) {
            TowerDefenseGameMap towerDefenseGameMap = (TowerDefenseGameMap) gameMap;

            return renderMap(towerDefenseGameMap);
        }

        log.error("The game map is not a TowerDefenseGameMap");
        return "";
    }

    @Override
    public String commandPrompt(GamePlayer gamePlayer) {
        TowerDefensePlayer towerDefensePlayer = (TowerDefensePlayer) gamePlayer;
        String color = (towerDefensePlayer.getPlayerType() == PlayerType.A) ? ANSI_BLUE_BRIGHT : ANSI_RED_BRIGHT;
        return color + "TowerDefensePlayer " + towerDefensePlayer.getPlayerType() + ": Enter command x <coord>,y <coord>,building type <0:Defense , 1:Attack, 2:Energy  or 3: Tesla>" + ANSI_RESET;
    }

    public String renderMap(TowerDefenseGameMap towerDefenseGameMap) {
        ThreeEntityCell[][] outputMap = new ThreeEntityCell[GameConfig.getMapHeight()][GameConfig.getMapWidth()];

        IntStream.range(0, GameConfig.getMapHeight())
                .forEach(y -> {
                            IntStream.range(0, GameConfig.getMapWidth())
                                    .forEach(x -> {
                                        int mirrorX = x;
                                        if (x >= GameConfig.getMapWidth() / 2) {
                                            mirrorX = GameConfig.getMapWidth() - 1 - x;
                                        }

                                        outputMap[y][x] = new ThreeEntityCell(y, mirrorX);
                                    });
                        }
                );

        towerDefenseGameMap.getBuildings()
                .forEach((Building b) -> {
                    String icon = b.isConstructed() ? b.getIcon() : b.getIcon().toLowerCase();
                    outputMap[b.getY()][b.getX()].setMiddle(
                            wrapPlayerColour(icon, b.getPlayerType())
                    );
                });


        towerDefenseGameMap.getMissiles()
                .forEach(p -> {
                    String icon = p.getIcon();
                    if (p.getDirection() == Direction.LEFT) {
                        outputMap[p.getY()][p.getX()].setLeft(
                                wrapPlayerColour(icon, p.getPlayerType())
                        );

                    } else if (p.getDirection() == Direction.RIGHT) {
                        outputMap[p.getY()][p.getX()].setRight(
                                wrapPlayerColour(icon, p.getPlayerType())
                        );
                    }
                });

        StringBuilder outputString = new StringBuilder();
        outputString.append("\n");

        TowerDefensePlayer towerDefensePlayerA = null;
        TowerDefensePlayer towerDefensePlayerB = null;

        try {
            towerDefensePlayerA = towerDefenseGameMap.getPlayer(PlayerType.A);
            towerDefensePlayerB = towerDefenseGameMap.getPlayer(PlayerType.B);
        } catch (Exception e) {
            log.error(e);
        }

        appendPlayerDetails(outputString, towerDefensePlayerA, ANSI_BLUE_BRIGHT);
        appendPlayerDetails(outputString, towerDefensePlayerB, ANSI_RED_BRIGHT);

        int[] maxLeftRightLength = new int[2];

        Arrays.stream(outputMap)
                .forEach(row -> {
                    Arrays.stream(row).forEach(cell -> {

                        int leftLength = StringUtils.countMatches(cell.getLeft(), "<");

                        if (leftLength > maxLeftRightLength[0]) {
                            maxLeftRightLength[0] = leftLength;
                        }

                        int rightLength = StringUtils.countMatches(cell.getRight(), ">");

                        if (rightLength > maxLeftRightLength[1]) {
                            maxLeftRightLength[1] = rightLength;
                        }
                    });
                });

        ThreeEntityCell shieldedCell = new ThreeEntityCell();
        boolean shieldExists = towerDefenseGameMap.shieldExists();
        towerDefenseGameMap.getTowerDefensePlayers()
                .forEach(p -> {
                    if (p.isIroncurtainActive()) {
                        if (p.getPlayerType() == PlayerType.A) {
                            shieldedCell.setShieldLeft("\\");
                        } else if (p.getPlayerType() == PlayerType.B) {
                            shieldedCell.setShieldRight("/");
                        }
                    }
                });

        Arrays.stream(outputMap)
                .map(row -> {
                    List<ThreeEntityCell> listRow = new ArrayList<>(Arrays.asList(row));
                    if (shieldExists) {
                        listRow.add(row.length / 2, shieldedCell);
                    }
                    return listRow;
                })
                .forEach(listRow -> {
                    StringBuilder rowString = new StringBuilder();
                    listRow.forEach(cell -> {
                        cell.padLeft(maxLeftRightLength[0]);
                        cell.padRight(maxLeftRightLength[1]);

                        rowString.append(cell);
                    });

                    outputString.append(rowString).append("\n");
                });

        return outputString.toString();
    }

    private void appendPlayerDetails(StringBuilder outputString, TowerDefensePlayer player, String ansiColour) {
        outputString.append(ansiColour)
                .append("Player ").append(player.getPlayerType())
                .append(", Health=").append(padLeftInt(player.getHealth(), 3))
                .append(", Energy=").append(padLeftInt(player.getEnergy(), 6))
                .append(", Score=").append(padLeftInt(player.getScore(), 6))
                .append(", IronCurtainAvailable= ").append(booleanToString(player.canPlaceIronCurtain()))
                .append(ANSI_RESET);
        outputString.append("\n");
    }

    public static String padRight(String s, int n) {
        return String.format("%1$-" + n + "s", s);
    }

    public static String padLeft(String s, int n) {
        return String.format("%1$" + n + "s", s);
    }

    public static String padLeftInt(int i, int n) {
        return padLeft(Integer.toString(i), n);
    }

    public static String booleanToString(boolean b) {
        return b ? "Yes" : "No";
    }

    public static String removeColour(String input) {
        List<String> ansiCodes = new ArrayList<>();
        ansiCodes.add(TowerDefenseConsoleMapRenderer.ANSI_RESET);
        ansiCodes.add(TowerDefenseConsoleMapRenderer.ANSI_GRAY);
        ansiCodes.add(TowerDefenseConsoleMapRenderer.ANSI_RED_BRIGHT);
        ansiCodes.add(TowerDefenseConsoleMapRenderer.ANSI_BLUE_BRIGHT);

        return ansiCodes.stream()
                .reduce(input, (s, s2) -> s.replace(s2, ""));
    }

    private String wrapPlayerColour(String icon, PlayerType id) {
        return id == PlayerType.A
                ? ANSI_BLUE_BRIGHT + icon + ANSI_RESET
                : id == PlayerType.B
                ? ANSI_RED_BRIGHT + icon + ANSI_RESET
                : icon;
    }
}

