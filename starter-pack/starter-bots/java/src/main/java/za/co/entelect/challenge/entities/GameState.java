package za.co.entelect.challenge.entities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GameState {
    protected Player[] players;
    protected CellStateContainer[][] gameMap;
    protected GameDetails gameDetails;


    public List<Player> getPlayers(){
        return new ArrayList<Player>(Arrays.asList(players));
    }

    public List<CellStateContainer> getGameMap(){
        ArrayList<CellStateContainer> list = new ArrayList<CellStateContainer>();

        for (int i = 0; i < gameMap.length; i++){
            for (int j = 0; j < gameMap[i].length; j ++){
                list.add(gameMap[i][j]);
            }
        }

        return list;
    }

    public GameDetails getGameDetails() {
        return gameDetails;
    }
}
