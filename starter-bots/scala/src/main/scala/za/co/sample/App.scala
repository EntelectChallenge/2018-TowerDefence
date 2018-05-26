package za.co.sample

import za.co.sample.models.Models.{Cell, GameDetails, Player, State}

import scala.util.Random

object App {
  def main(args: Array[String]): Unit = {
    val stateFile = "state.json"
    val commandFile = "command.txt"
    implicit val NOTHING_COMMAND = ""

    val stateRaw = Parser.readFile(stateFile)
    val state: State = Parser.readJsonParse4s(stateRaw)

    implicit val gameDetails = state.gameDetails

    implicit val gameMap = state.gameMap

    implicit val mapHeight = gameDetails.mapHeight
    implicit val mapWidth = gameDetails.mapWidth

    val playerA = state.players.filter(_.playerType == "A").head
    val playerB = state.players.filter(_.playerType == "B").head

    implicit val expensiveBuildingCost = List(gameDetails.buildingsStats.ATTACK.price, gameDetails.buildingsStats.DEFENSE.price, gameDetails.buildingsStats.ENERGY.price).max

    val action = {
      if(isUnderAttack(playerA, playerB) && canAffordBuilding("DEFENSE", playerA))
        placeDefendingBuilding(playerA, playerB)
      else if (playerA.energy > expensiveBuildingCost)
        placeRandomBuilding(playerA)
      else
        doNothingCommand
    }

    Parser.writeToFile(commandFile, action)
  }

  /**
    * Chooses a random unoccupied spot in row to place a defending building.
    *
    * @return String
    */
  def placeDefendingBuilding(playerA: Player, playerB: Player)(implicit gameMap: List[List[Cell]]): String = {
    val gettingAttackedInRow = gameMap.filter(row => {
      val attacking = row.exists(cell => cell.cellOwner == playerB.playerType && cell.buildings.exists(building => building.buildingType == "A" || building.buildingType == "a"))
      val defending = row.exists(cell => cell.cellOwner == playerA.playerType && cell.buildings.exists(building => building.buildingType == "d" || building.buildingType == "D"))
      attacking && !defending
    }).head.head.y

    val cells = gameMap.flatMap(row => row.filter(cell => cell.cellOwner == playerA.playerType && cell.buildings.isEmpty && cell.y == gettingAttackedInRow))
    val chosenCell = cells(Random.nextInt(cells.size))
    s"${chosenCell.x},${chosenCell.y},0"
  }

  /**
    * Chooses a random unoccupied cell owned by Player A, and place a random building.
    *
    * @return String
    */
  def placeRandomBuilding(playerA: Player)(implicit gameMap: List[List[Cell]]): String = {
    val unoccupiedCells = gameMap.flatMap(row => row.filter(cell => cell.cellOwner == playerA.playerType && cell.buildings.isEmpty))
    val chosenCell = unoccupiedCells(Random.nextInt(unoccupiedCells.size))
    s"${chosenCell.x},${chosenCell.y},${Random.nextInt(2)}"
  }

  /**
    * Check if Player A is under attack by Player B and whether Player A has no defending buildings in that row.
    *
    * @return Boolean
    */
  def isUnderAttack(playerA: Player, playerB: Player)(implicit gameMap: List[List[Cell]]): Boolean = {
    val gettingAttackedInRow = gameMap.map(row => {
      val attacking = row.exists(cell => cell.cellOwner == playerB.playerType && cell.buildings.exists(_.buildingType == "ATTACK"))
      val defending = row.exists(cell => cell.cellOwner == playerA.playerType && cell.buildings.exists(_.buildingType == "DEFENSE"))
      attacking && !defending
    })
    gettingAttackedInRow.exists(attacked => attacked)
  }


  /**
    * Checks if a building type is affordable.
    *
    * @param buildingType
    * @return Boolean
    */
  def canAffordBuilding(buildingType: String, playerA: Player)(implicit gameDetails: GameDetails): Boolean = {
    buildingType match {
      case "DEFENSE" => gameDetails.buildingPrices.DEFENSE < playerA.energy
      case "ATTACK" => gameDetails.buildingPrices.ATTACK < playerA.energy
      case "ENERGY" => gameDetails.buildingPrices.ENERGY < playerA.energy
    }
  }

  /**
    * Do nothing.
    *
    * @return NOTHING
    */
  def doNothingCommand(implicit NOTHING_COMMAND: String): String = {
    NOTHING_COMMAND
  }
}
