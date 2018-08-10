package za.co.entelect.challenge.core.renderers;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.core.entities.CellStateContainer;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.enums.PlayerType;

import java.util.stream.IntStream;

public class RendererHelper {


    public static CellStateContainer[][] renderPlayerA(TowerDefenseGameMap towerDefenseGameMap) {
        CellStateContainer[][] outputMap = generateEmptyBoard();

        towerDefenseGameMap.getBuildings()
                .forEach(b -> {
                    outputMap[b.getY()][b.getX()].buildings.add(b);
                });

        towerDefenseGameMap.getMissiles()
                .forEach(p -> {
                    outputMap[p.getY()][p.getX()].missiles.add(p);
                });

        return outputMap;
    }

    public static CellStateContainer[][] renderPlayerB(TowerDefenseGameMap towerDefenseGameMap) {
        CellStateContainer[][] outputMap = generateEmptyBoard();

        towerDefenseGameMap.getBuildings()
                .forEach(b -> {
                    outputMap[b.getY()][GameConfig.getMapWidth() - b.getX() - 1].buildings.add(b.getInvertedXInstance());
                });

        towerDefenseGameMap.getMissiles()
                .forEach(p -> {
                    outputMap[p.getY()][GameConfig.getMapWidth() - p.getX() - 1].missiles.add(p.getInvertedXInstance());
                });

        return outputMap;
    }

    private static CellStateContainer[][] generateEmptyBoard() {
        CellStateContainer[][] outputMap = new CellStateContainer[GameConfig.getMapHeight()][GameConfig.getMapWidth()];

        IntStream.range(0, GameConfig.getMapHeight())
                .forEach(y -> {
                            IntStream.range(0, GameConfig.getMapWidth())
                                    .forEach(x -> {
                                        PlayerType renderedPlayerType = (x >= GameConfig.getMapWidth() / 2 ? PlayerType.B : PlayerType.A);

                                        outputMap[y][x] = new CellStateContainer(x, y, renderedPlayerType);
                                    });
                        }
                );

        return outputMap;
    }
}
