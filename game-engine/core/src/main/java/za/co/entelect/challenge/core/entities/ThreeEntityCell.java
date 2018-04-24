package za.co.entelect.challenge.core.entities;

import org.apache.commons.lang3.StringUtils;
import za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer;

import static za.co.entelect.challenge.core.renderers.TowerDefenseConsoleMapRenderer.removeColour;

public class ThreeEntityCell {

    private String left;
    private String middle;
    private String right;

    public ThreeEntityCell() {
        this.left = " ";
        this.middle = " ";
        this.right = " ";
    }

    public ThreeEntityCell(Integer y, Integer x) {
        this.left = TowerDefenseConsoleMapRenderer.ANSI_GRAY + x.toString() + TowerDefenseConsoleMapRenderer.ANSI_RESET;
        this.middle = " ";
        this.right = TowerDefenseConsoleMapRenderer.ANSI_GRAY + y.toString() + TowerDefenseConsoleMapRenderer.ANSI_RESET;
    }

    public void setRight(String right) {
        if (StringUtils.isNumeric(removeColour(this.right))) {
            this.right = right;
        } else {
            this.right = this.right + right;
        }
    }

    @Override
    public String toString() {
        return "[" + left + middle + right + "]";
    }

    public void setLeft(String left) {
        if (StringUtils.isNumeric(removeColour(this.left))) {
            this.left = left;
        } else {
            this.left = this.left + left;
        }
    }

    public void setMiddle(String middle) {
        this.middle = middle;
    }

    public String getLeft() {
        return left;
    }

    public String getRight() {
        return right;
    }

    public void padLeft(int maxLeftLength) {
        int length = removeColour(left).length();

        while (length < maxLeftLength) {
            left = String.format("%s ", left);
            length = removeColour(left).length();
        }
    }

    public void padRight(int maxRightLength) {
        int length = removeColour(right).length();

        while (length < maxRightLength) {
            right = String.format(" %s", right);
            length = removeColour(right).length();
        }
    }

}
