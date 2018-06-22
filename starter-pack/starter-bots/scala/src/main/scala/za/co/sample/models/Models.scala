package za.co.sample.models

object Models{
  case class State(gameDetails: GameDetails,
                               players: List[Player],
                               gameMap: List[List[Cell]])

  case class GameDetails(round: Int,
                         mapWidth: Int,
                         mapHeight: Int,
                         roundIncomeEnergy: Int,
                         buildingPrices: BuildingPrices,
                         buildingsStats: BuildingsStats)

  case class BuildingPrices(DEFENSE: Int,
                            ATTACK: Int,
                            ENERGY: Int)

  case class BuildingsStats(DEFENSE: BuildingStats,
                            ATTACK: BuildingStats,
                            ENERGY: BuildingStats)

  case class BuildingStats(health: Int,
                           constructionTime: Int,
                           price: Int,
                           weaponDamage: Int,
                           weaponSpeed: Int,
                           weaponCooldownPeriod: Int,
                           energyGeneratedPerTurn: Int,
                           destroyMultiplier: Int,
                           constructionScore: Int)

  case class Player(playerType: String,
                    energy: Int,
                    health: Int,
                    hitsTaken: Int,
                    score: Int)

  case class Cell(x: Int,
                  y: Int,
                  buildings: List[Building],
                  missiles: List[Missile],
                  cellOwner: String)

  case class Building(health: Int,
                      constructionTimeLeft: Int,
                      price: Int,
                      weaponDamage: Int,
                      weaponSpeed: Int,
                      weaponCooldownTimeLeft: Int,
                      weaponCooldownPeriod: Int,
                      destroyMultiplier: Int,
                      constructionScore: Int,
                      energyGeneratedPerTurn: Int,
                      buildingType: String,
                      x: Int,
                      y: Int,
                      playerType: String)

  case class Missile(damage: Int,
                     speed: Int,
                     x: Int,
                     y: Int,
                     playerType: String)
}
