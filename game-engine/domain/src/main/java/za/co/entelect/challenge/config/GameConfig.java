package za.co.entelect.challenge.config;

import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.builder.fluent.Configurations;
import org.apache.commons.configuration2.ex.ConfigurationException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class GameConfig {

    private static Configuration configuration;
    private static final Logger log = LogManager.getLogger(GameConfig.class);

    public static void initConfig(String configLocation) {
        if (configuration == null) {
            Configurations configurations = new Configurations();

            try {
                configuration = configurations.properties(configLocation);

            } catch (ConfigurationException e) {
                log.error("Unable to initialise configuration, please have a look at the inner exception.", e);
                throw new RuntimeException("Unable to initialise configuration, please have a look at the inner exception.", e);
            }
        }
    }

    public static int getMapWidth() {
        return configuration.getInt("game.config.map-width");
    }

    public static int getMapHeight() {
        return configuration.getInt("game.config.map-height");
    }

    public static int getMaxRounds() {
        return configuration.getInt("game.config.max-rounds");
    }

    public static int getStartEnergy() {
        return configuration.getInt("game.config.start-energy");
    }

    public static int getRoundIncomeEnergy() {
        return configuration.getInt("game.config.round-income-energy");
    }

    public static int getStartingHealth() {
        return configuration.getInt("game.config.starting-health");
    }

    public static int getHealthScoreMultiplier() {
        return configuration.getInt("game.config.health-score-multiplier");
    }

    public static int getEnergyScoreMultiplier() {
        return configuration.getInt("game.config.energy-score-multiplier");
    }

    public static int getDefenseHealth() {
        return configuration.getInt("game.config.defense.config.health");
    }

    public static int getDefenseConstructionTimeLeft() {
        return configuration.getInt("game.config.defense.config.construction-time-left");
    }

    public static int getDefensePrice() {
        return configuration.getInt("game.config.defense.config.price");
    }

    public static int getDefenseWeaponDamage() {
        return configuration.getInt("game.config.defense.config.weapon-damage");
    }

    public static int getDefenseWeaponSpeed() {
        return configuration.getInt("game.config.defense.config.weapon-speed");
    }

    public static int getDefenseWeaponCooldownPeriod() {
        return configuration.getInt("game.config.defense.config.weapon-cooldown-period");
    }

    public static String getDefenseIcon() {
        return configuration.getString("game.config.defense.config.icon");
    }

    public static int getDefenseDestroyMultiplier() {
        return configuration.getInt("game.config.defense.config.destroy-multiplier");
    }

    public static int getDefenseConstructionScore() {
        return configuration.getInt("game.config.defense.config.construction-score");
    }

    public static int getDefenseEnergyPerTurn() {
        return configuration.getInt("game.config.defense.config.energy-Produced-per-turn");
    }


    public static int getEnergyHealth() {
        return configuration.getInt("game.config.energy.config.health");
    }

    public static int getEnergyConstructionTimeLeft() {
        return configuration.getInt("game.config.energy.config.construction-time-left");
    }

    public static int getEnergyPrice() {
        return configuration.getInt("game.config.energy.config.price");
    }

    public static int getEnergyWeaponDamage() {
        return configuration.getInt("game.config.energy.config.weapon-damage");
    }

    public static int getEnergyWeaponSpeed() {
        return configuration.getInt("game.config.energy.config.weapon-speed");
    }

    public static int getEnergyWeaponCooldownPeriod() {
        return configuration.getInt("game.config.energy.config.weapon-cooldown-period");
    }

    public static String getEnergyIcon() {
        return configuration.getString("game.config.energy.config.icon");
    }

    public static int getEnergyDestroyMultiplier() {
        return configuration.getInt("game.config.energy.config.destroy-multiplier");
    }

    public static int getEnergyConstructionScore() {
        return configuration.getInt("game.config.energy.config.construction-score");
    }

    public static int getEnergyEnergyPerTurn() {
        return configuration.getInt("game.config.energy.config.energy-Produced-per-turn");
    }


    public static int getAttackHealth() {
        return configuration.getInt("game.config.attack.config.health");
    }

    public static int getAttackConstructionTimeLeft() {
        return configuration.getInt("game.config.attack.config.construction-time-left");
    }

    public static int getAttackPrice() {
        return configuration.getInt("game.config.attack.config.price");
    }

    public static int getAttackWeaponDamage() {
        return configuration.getInt("game.config.attack.config.weapon-damage");
    }

    public static int getAttackWeaponSpeed() {
        return configuration.getInt("game.config.attack.config.weapon-speed");
    }

    public static int getAttackWeaponCooldownPeriod() {
        return configuration.getInt("game.config.attack.config.weapon-cooldown-period");
    }

    public static String getAttackIcon() {
        return configuration.getString("game.config.attack.config.icon");
    }


    public static int getAttackDestroyMultiplier() {
        return configuration.getInt("game.config.attack.config.destroy-multiplier");
    }

    public static int getAttackConstructionScore() {
        return configuration.getInt("game.config.attack.config.construction-score");
    }

    public static int getAttackEnergyPerTurn() {
        return configuration.getInt("game.config.attack.config.energy-Produced-per-turn");
    }
}
