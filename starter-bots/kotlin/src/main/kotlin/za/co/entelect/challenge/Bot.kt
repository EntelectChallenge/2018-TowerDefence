package za.co.entelect.challenge

import java.util.ArrayList
import java.util.Random

class Bot(private val gameState: GameState) {
    private val gameDetails: GameDetails = gameState.gameDetails
    private val gameWidth: Int
    private val gameHeight: Int
    private val myself: Player
    private val opponent: Player
    private val buildings: List<Building>
    private val missiles: List<Missile>

    //if enemy has an attack building and I don't have a blocking wall
    private val isUnderAttack: Boolean
        get() {
            for (i in 0 until gameHeight) {
                val opponentAttacksCount = getAllBuildingsForPlayer(PlayerType.B, ::isAttackBuilding, i).size
                val myDefenseCount = getAllBuildingsForPlayer(PlayerType.A, ::isDefenceBuilding, i).size
                if (opponentAttacksCount > 0 && myDefenseCount == 0) {
                    return true
                }
            }
            return false
        }

    init {
        //        energyPerTurn = gameDetails.;
        gameWidth = gameDetails.mapWidth
        gameHeight = gameDetails.mapHeight
        myself = gameState.players.first { p -> p.playerType == PlayerType.A }
        opponent = gameState.players.first { p -> p.playerType == PlayerType.B }
        buildings = gameState.gameMap.flatten().map { cellStateContainer -> cellStateContainer.buildings }.flatten()
        missiles = gameState.gameMap.flatten().map { cellStateContainer -> cellStateContainer.missiles }.flatten()
    }

    fun run(): String {
        return when {
            isUnderAttack -> defendRow()
            hasEnoughEnergyForMostExpensiveBuilding() -> buildRandom()
            else -> doNothingCommand()
        }
    }

    private fun buildRandom(): String {
        val emptyCells = gameState.gameMap.flatten()
                .filter { c -> c.buildings.isEmpty() && c.x < gameWidth / 2 }
        if (emptyCells.isEmpty()) {
            return doNothingCommand()
        }
        val randomEmptyCell = getRandomElementOfList(emptyCells)
        val buildingTypes = ArrayList(gameDetails.buildingPrices.keys)
        val randomBuildingType = getRandomElementOfList(buildingTypes)
        return if (!canAffordBuilding(randomBuildingType)) {
            doNothingCommand()
        } else buildCommand(randomEmptyCell.x, randomEmptyCell.y, randomBuildingType)
    }

    private fun hasEnoughEnergyForMostExpensiveBuilding(): Boolean {
        return gameDetails.buildingPrices.values.stream()
                .filter { bp -> bp < myself.energy }
                .toArray().size == 3
    }

    private fun defendRow(): String {
        for (i in 0 until gameHeight) {
            val opponentAttacksCount = getAllBuildingsForPlayer(PlayerType.B, { b -> b.buildingType == BuildingType.ATTACK }, i).size
            if (opponentAttacksCount > 0 && canAffordBuilding(BuildingType.DEFENSE)) {
                return placeBuildingInRow(BuildingType.DEFENSE, i)
            }
        }
        return buildRandom()
    }

    private fun doNothingCommand(): String {
        return ""
    }

    private fun placeBuildingInRow(buildingType: BuildingType, y: Int): String {
        val emptyCells = gameState.gameMap.flatten()
                .filter {
                    (it.buildings.isEmpty()
                            && it.y == y
                            && it.x < gameWidth / 2 - 1)
                }
        if (emptyCells.isEmpty()) {
            return buildRandom()
        }
        val randomEmptyCell = getRandomElementOfList(emptyCells)
        return buildCommand(randomEmptyCell.x, randomEmptyCell.y, buildingType)
    }

    private fun <T> getRandomElementOfList(list: List<T>): T {
        return list[Random().nextInt(list.size)]
    }

    private fun buildCommand(x: Int, y: Int, buildingType: BuildingType): String {
        val buildCommand = StringBuilder()

        buildCommand.append(x)
        buildCommand.append(",")
        buildCommand.append(y)
        buildCommand.append(",")
        when (buildingType) {

            BuildingType.DEFENSE -> buildCommand.append("0")
            BuildingType.ATTACK -> buildCommand.append("1")
            BuildingType.ENERGY -> buildCommand.append("2")
        }
        return buildCommand.toString()
    }

    private fun getAllBuildingsForPlayer(playerType: PlayerType, filter: (Building) -> Boolean, y: Int): List<Building> {
        return buildings
                .filter { b -> b.playerType == playerType && b.y == y }
                .filter(filter)
    }

    private fun canAffordBuilding(buildingType: BuildingType): Boolean {
        val cost = when (buildingType) {
            BuildingType.DEFENSE -> gameDetails.buildingPrices.getOrDefault(BuildingType.DEFENSE, 100000)
            BuildingType.ATTACK -> gameDetails.buildingPrices.getOrDefault(BuildingType.ATTACK, 100000)
            BuildingType.ENERGY -> gameDetails.buildingPrices.getOrDefault(BuildingType.ENERGY, 100000)
        }
        return myself.energy >= cost
    }

}
