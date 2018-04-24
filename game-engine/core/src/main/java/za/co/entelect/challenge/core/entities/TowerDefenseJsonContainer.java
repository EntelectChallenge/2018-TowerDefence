package za.co.entelect.challenge.core.entities;

public class TowerDefenseJsonContainer {
    protected GameDetails gameDetails;
    protected PlayerData[] players;
    protected CellStateContainer[][] gameMap;

    public TowerDefenseJsonContainer(CellStateContainer[][] gameMap, PlayerData[] players, int round){
        gameDetails = new GameDetails(round);
        this.players = players;
        this.gameMap = gameMap;
    }
}
