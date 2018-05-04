package za.co.entelect.challenge

import java.util.HashMap

data class Building(
        val x: Int, val y: Int, val playerType: PlayerType,
        val health: Int,
        val constructionTimeLeft: Int,
        val price: Int,
        val weaponDamage: Int,
        val weaponSpeed: Int,
        val weaponCooldownTimeLeft: Int,
        val weaponCooldownPeriod: Int,
        val destroyMultiplier: Int,
        val constructionScore: Int,
        val energyGeneratedPerTurn: Int,
        val buildingType: BuildingType)

data class Missile(
        val x: Int, val y: Int, val playerType: PlayerType,
        val damage: Int,
        val speed: Int)

data class Player(
        val playerType: PlayerType,
        val energy: Int,
        val health: Int,
        val hitsTaken: Int,
        val score: Int)

data class CellStateContainer(
        val x: Int, val y: Int, val playerType: PlayerType,
        val buildings: List<Building>,
        val missiles: List<Missile>)

data class GameDetails(
    val round: Int,
    val mapWidth: Int,
    val mapHeight: Int,
    val buildingPrices: HashMap<BuildingType, Int>)

data class GameState(
        val players: Array<Player>,
        val gameMap: Array<Array<CellStateContainer>>,
        var gameDetails: GameDetails)
