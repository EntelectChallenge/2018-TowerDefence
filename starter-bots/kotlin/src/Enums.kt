package za.co.entelect.challenge

enum class BuildingType {
    DEFENSE,
    ATTACK,
    ENERGY
}

fun isAttackBuilding(building: Building) : Boolean {
    return building.buildingType == BuildingType.ATTACK
}

fun isDefenceBuilding(building: Building) : Boolean {
    return building.buildingType == BuildingType.DEFENSE
}

enum class Direction private constructor(val multiplier: Int) {
    LEFT(-1),
    RIGHT(1)
}

enum class PlayerType {
    A,
    B
}