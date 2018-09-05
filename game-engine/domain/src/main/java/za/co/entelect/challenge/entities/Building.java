package za.co.entelect.challenge.entities;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.enums.BuildingType;
import za.co.entelect.challenge.enums.PlayerType;

public class Building extends Cell {

    private int health;
    private int constructionTimeLeft;
    private int price;
    private int weaponDamage;
    private int weaponSpeed;
    private int weaponCooldownTimeLeft;
    private int weaponCooldownPeriod;
    private transient String icon;
    private int destroyMultiplier;
    private int constructionScore;
    private int energyGeneratedPerTurn;
    private int maxRange;
    private BuildingType buildingType;
    private int energyPerShot;

    public Building(int x,
                    int y,
                    PlayerType playerType,
                    int health,
                    int constructionTimeLeft,
                    int price,
                    int weaponDamage,
                    int weaponSpeed,
                    int weaponCooldownPeriod,
                    String icon,
                    int destroyMultiplier,
                    int constructionScore,
                    int energyGeneratedPerTurn,
                    int maxRange,
                    BuildingType buildingType) {
        super(x, y, playerType);
        this.health = health;
        this.constructionTimeLeft = constructionTimeLeft;
        this.price = price;
        this.weaponDamage = weaponDamage;
        this.weaponSpeed = weaponSpeed;
        this.destroyMultiplier = destroyMultiplier;
        this.constructionScore = constructionScore;
        this.weaponCooldownTimeLeft = 0;
        this.weaponCooldownPeriod = weaponCooldownPeriod;
        this.icon = icon;
        this.energyGeneratedPerTurn = energyGeneratedPerTurn;
        this.maxRange = maxRange;
        this.buildingType = buildingType;
    }

    public Building(int x,
                    int y,
                    PlayerType playerType,
                    int health,
                    int constructionTimeLeft,
                    int price,
                    int weaponDamage,
                    int weaponSpeed,
                    int weaponCooldownPeriod,
                    String icon,
                    int destroyMultiplier,
                    int constructionScore,
                    int energyGeneratedPerTurn,
                    int maxRange,
                    int energyPerShot,
                    BuildingType buildingType) {
        super(x, y, playerType);
        this.health = health;
        this.constructionTimeLeft = constructionTimeLeft;
        this.price = price;
        this.weaponDamage = weaponDamage;
        this.weaponSpeed = weaponSpeed;
        this.destroyMultiplier = destroyMultiplier;
        this.constructionScore = constructionScore;
        this.weaponCooldownTimeLeft = 0;
        this.weaponCooldownPeriod = weaponCooldownPeriod;
        this.icon = icon;
        this.energyGeneratedPerTurn = energyGeneratedPerTurn;
        this.maxRange = maxRange;
        this.energyPerShot = energyPerShot;
        this.buildingType = buildingType;
    }

    //Used for rendering
    public Building getInvertedXInstance() {
        PlayerType type;
        if (this.playerType == PlayerType.A) {
            type = PlayerType.B;
        } else {
            type = PlayerType.A;
        }
        Building newBuilding = new Building(GameConfig.getMapWidth() - this.x - 1,
                this.y,
                type,
                this.health,
                this.constructionTimeLeft,
                this.price,
                this.weaponDamage,
                this.weaponSpeed,
                this.weaponCooldownPeriod,
                this.icon,
                this.destroyMultiplier,
                this.constructionScore,
                this.energyGeneratedPerTurn,
                this.maxRange,
                this.energyPerShot,
                this.buildingType);

        newBuilding.weaponCooldownTimeLeft = this.weaponCooldownTimeLeft;

        return newBuilding;
    }

    public void damageSelf(Missile m, TowerDefensePlayer missileOwner) {
        int damageTaken = Math.min(health, m.getDamage());

        setHealthAndAddScore(damageTaken, missileOwner);
    }

    public void damageSelfDirectly(int damageTaken, TowerDefensePlayer missileOwner) {
        setHealthAndAddScore(damageTaken, missileOwner);
    }

    private void setHealthAndAddScore(int damageTaken, TowerDefensePlayer missileOwner) {
        int previousHealth = health;
        health -= damageTaken;
        health = Math.max(0, health);

        missileOwner.addScore((previousHealth - health) * destroyMultiplier);
    }

    public void decreaseCooldown() {
        weaponCooldownTimeLeft--;
    }

    public void resetCooldown() {
        weaponCooldownTimeLeft = weaponCooldownPeriod;
    }

    public int getConstructionTimeLeft() {
        return constructionTimeLeft; // construction happens on same round as placement
    }

    public void decreaseConstructionTimeLeft() {
        constructionTimeLeft--;
    }

    public boolean isConstructed() {
        return (getConstructionTimeLeft() <= -1);
    }

    public int getHealth() {
        return health;
    }

    public int getPrice() {
        return price;
    }

    public int getWeaponDamage() {
        return weaponDamage;
    }

    public int getWeaponSpeed() {
        return weaponSpeed;
    }

    public int getWeaponCooldownTimeLeft() {
        return weaponCooldownTimeLeft;
    }

    public int getEnergyGeneratedPerTurn() {
        return energyGeneratedPerTurn;
    }

    public int getWeaponCooldownPeriod() {
        return weaponCooldownPeriod;
    }

    public BuildingType getBuildingType() {
        return buildingType;
    }

    public String getIcon() {
        return icon;
    }

    public int getDestroyMultiplier() {
        return destroyMultiplier;
    }

    public int getConstructionScore() {
        return constructionScore;
    }

    public int getMaxRange() {
        return maxRange;
    }

    public int getEnergyPerShot() {
        return energyPerShot;
    }
}
