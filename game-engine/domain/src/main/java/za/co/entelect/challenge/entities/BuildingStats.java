package za.co.entelect.challenge.entities;

public class BuildingStats {

    public int health;
    public int constructionTime;
    public int price;
    public int weaponDamage;
    public int weaponSpeed;
    public int weaponCooldownPeriod;
    public int energyGeneratedPerTurn;
    public int destroyMultiplier;
    public int constructionScore;

    public BuildingStats(Building building) {
        this.health = building.getHealth();
        this.constructionTime = building.getConstructionTimeLeft();
        this.price = building.getPrice();
        this.weaponDamage = building.getWeaponDamage();
        this.weaponSpeed = building.getWeaponSpeed();
        this.weaponCooldownPeriod = building.getWeaponCooldownPeriod();
        this.destroyMultiplier = building.getDestroyMultiplier();
        this.constructionScore = building.getConstructionScore();
        this.energyGeneratedPerTurn = building.getEnergyGeneratedPerTurn();
    }

    public static String getTextHeader() {
        return "health;constructionTime;price;weaponDamage;weaponSpeed;weaponCooldownPeriod;energyGeneratedPerTurn;destroyMultiplier;constructionScore";
    }

    @Override
    public String toString() {
        return health + ";" +
                constructionTime + ";" +
                price + ";" +
                weaponDamage + ";" +
                weaponSpeed + ";" +
                weaponCooldownPeriod + ";" +
                energyGeneratedPerTurn + ";" +
                destroyMultiplier + ";" +
                constructionScore + ";";
    }
}
