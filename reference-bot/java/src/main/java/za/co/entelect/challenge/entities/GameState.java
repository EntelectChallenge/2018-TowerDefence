package za.co.entelect.challenge.entities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GameState {
    public GameDetails gameDetails;
    protected Player[] players;
    protected CellStateContainer[][] gameMap;

    public List<Player> getPlayers() {
        return new ArrayList<>(Arrays.asList(players));
    }

    public List<CellStateContainer> getGameMap() {
        ArrayList<CellStateContainer> list = new ArrayList<>();

        for (CellStateContainer[] aGameMap : gameMap) {
            list.addAll(Arrays.asList(aGameMap));
        }

        return list;
    }
}
