package za.co.entelect.challenge.commands;

import za.co.entelect.challenge.config.GameConfig;
import za.co.entelect.challenge.enums.PlayerType;
import za.co.entelect.challenge.game.contracts.exceptions.InvalidCommandException;

public class CommandHelpers {

    public static boolean isCommandInBounds(Integer positionX, Integer positionY) {
        int mapWidth = GameConfig.getMapWidth();
        int mapHeight = GameConfig.getMapHeight();

        if ((positionX >= (mapWidth / 2) || positionX < 0) || (positionY >= (mapHeight) || positionY < 0)) {
            return false;
        }
        return true;
    }

    public static Integer mirrorXIndex(PlayerType id, Integer positionX) {
        if (id == PlayerType.B) {
            return GameConfig.getMapWidth() - 1 - positionX;
        }
        return positionX;
    }
}
