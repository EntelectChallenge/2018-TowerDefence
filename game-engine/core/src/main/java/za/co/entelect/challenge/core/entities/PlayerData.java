package za.co.entelect.challenge.core.entities;

import za.co.entelect.challenge.enums.PlayerType;

public class PlayerData {
    protected PlayerType playerType;
    protected int energy;
    protected int health;
    protected int hitsTaken;
    protected int score;

    public PlayerData(PlayerType playerType, int energy, int health, int hitsTaken, int score){
        this.playerType = playerType;
        this.energy = energy;
        this.health = health;
        this.hitsTaken = hitsTaken;
        this.score = score;
    }
}
