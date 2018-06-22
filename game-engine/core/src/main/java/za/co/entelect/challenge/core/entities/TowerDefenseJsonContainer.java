package za.co.entelect.challenge.core.entities;

import za.co.entelect.challenge.entities.Cell;

import java.util.ArrayList;
import java.util.List;

public class TowerDefenseJsonContainer {
    
    protected GameDetails gameDetails;
    protected PlayerData[] players;
    protected CellStateContainer[][] gameMap;
    protected ArrayList<List<Cell>> teslaHitList;

    public TowerDefenseJsonContainer(CellStateContainer[][] gameMap, PlayerData[] players, int round, ArrayList<List<Cell>> teslaHitList) {
        this.gameDetails = new GameDetails(round);
        this.players = players;
        this.gameMap = gameMap;
        this.teslaHitList = teslaHitList;
    }
}
