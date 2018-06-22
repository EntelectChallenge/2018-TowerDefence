package za.co.entelect.challenge.entities;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.enums.Direction;
import za.co.entelect.challenge.enums.PlayerType;

import java.util.UUID;

public class Missile extends Cell {

    private int damage;
    private int speed;
    private String id;
    private transient Direction direction;
    private transient String icon;
    private transient int unprocessedMovement;

    private void setDefaults() {
        this.id = UUID.randomUUID().toString();
        this.unprocessedMovement = this.speed;
    }

    public Missile(int x, int y, int damage, int speed, Direction direction, PlayerType playerType) {
        this.x = x;
        this.y = y;
        this.damage = damage;
        this.speed = speed;
        this.direction = direction;
        this.playerType = playerType;
        this.icon = (direction == Direction.LEFT) ? "<" : ">";
        setDefaults();
    }

    public Missile getInvertedXInstance(){
        PlayerType type;
        if (this.playerType == PlayerType.A){
            type = PlayerType.B;
        }else{
            type = PlayerType.A;
        }

        Missile newMissile = new Missile( GameConfig.getMapWidth() - this.x - 1,
                this.y,
                this.damage,
                this.speed,
                this.direction,
                type);

        newMissile.id = this.id;
        newMissile.direction = (this.direction == Direction.LEFT) ? Direction.RIGHT : Direction.LEFT;
        newMissile.icon = (newMissile.direction == Direction.LEFT) ? "<" : ">";

        return newMissile;
    }


    Missile(Building b, Direction direction) {
        super(b.getX(), b.getY(), b.getPlayerType());
        this.damage = b.getWeaponDamage();
        this.speed = b.getWeaponSpeed();
        this.direction = direction;
        this.icon = (direction == Direction.LEFT) ? "<" : ">";
        setDefaults();
    }

    public int getSpeed() {
        return speed;
    }

    public Direction getDirection() {
        return direction;
    }

    public int getDamage() {
        return damage;
    }

    public String getIcon() {
        return icon;
    }

    public void setSpeed(int newSpeed) {
        speed = newSpeed;
    }

    public void moveX(int numberOfCellsToMove) {
        x += numberOfCellsToMove;
    }

    public int getUnprocessedMovement() {
        return unprocessedMovement;
    }

    public void reduceUnprocessedMovement() {
        this.unprocessedMovement--;
    }

    public void resetUnprocessedMovement() {
        this.unprocessedMovement = this.speed;
    }
}
