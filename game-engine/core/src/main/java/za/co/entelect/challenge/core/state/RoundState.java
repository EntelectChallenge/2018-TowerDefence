package za.co.entelect.challenge.core.state;

import za.co.entelect.challenge.entities.TowerDefensePlayer;

public class RoundState {

    private String GameVersion;
    private String GameLevel;
    private String Round;
    private String MapDimension;
    private String Phase;
    private TowerDefensePlayer towerDefensePlayerThis;
    private TowerDefensePlayer towerDefensePlayerOther;
    private String map;


    @Override
    public String toString() {
        return "RoundState{" +
                "GameVersion='" + GameVersion + '\'' +
                ", GameLevel='" + GameLevel + '\'' +
                ", Round='" + Round + '\'' +
                ", MapDimension='" + MapDimension + '\'' +
                ", Phase='" + Phase + '\'' +
                ", PlayerA=" + towerDefensePlayerThis +
                ", PlayerB=" + towerDefensePlayerOther +
                '}';
    }
}
