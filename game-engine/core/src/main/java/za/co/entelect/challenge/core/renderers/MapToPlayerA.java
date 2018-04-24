package za.co.entelect.challenge.core.renderers;

import com.google.gson.Gson;
import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.core.entities.CellStateContainer;
import za.co.entelect.challenge.entities.TowerDefenseGameMap;
import za.co.entelect.challenge.enums.PlayerType;

import java.util.stream.IntStream;

public class MapToPlayerA {

    public String renderMap(TowerDefenseGameMap towerDefenseGameMap) {

        CellStateContainer[][] outputMap = new CellStateContainer[GameConfig.getMapHeight()][GameConfig.getMapWidth()];

        IntStream.range(0, GameConfig.getMapHeight())
                .forEach(y -> {
                            IntStream.range(0, GameConfig.getMapWidth())
                                    .forEach(x -> {
                                        outputMap[y][x] = new CellStateContainer(x, y, PlayerType.A);
                                    });
                        }
                );

        towerDefenseGameMap.getBuildings()
                .forEach(b -> {
                    outputMap[b.getY()][b.getX()].buildings.add(b);
                });

        towerDefenseGameMap.getMissiles()
                .forEach(p -> {
                    outputMap[p.getY()][p.getX()].missiles.add(p);
                });

        Gson gson = new Gson();
        return gson.toJson(outputMap);
    }

}
