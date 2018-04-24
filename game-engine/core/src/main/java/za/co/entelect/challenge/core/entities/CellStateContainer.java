package za.co.entelect.challenge.core.entities;

import za.co.entelect.challenge.entities.Building;
import za.co.entelect.challenge.entities.Missile;
import za.co.entelect.challenge.enums.PlayerType;

import java.util.ArrayList;
import java.util.List;

public class CellStateContainer {

    public int x;
    public int y;
    public List<Building> buildings;
    public List<Missile> missiles;
    public PlayerType cellOwner;

    public CellStateContainer(int x, int y, PlayerType cellOwner) {
        this.x = x;
        this.y = y;
        this.cellOwner = cellOwner;
        this.buildings = new ArrayList<>();
        this.missiles = new ArrayList<>();
    }
}
