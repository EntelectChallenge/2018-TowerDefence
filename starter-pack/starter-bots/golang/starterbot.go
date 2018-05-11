package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"math/rand"
	"time"
)

const (
	Defense string = "DEFENSE"
	Attack  string = "ATTACK"
	Energy  string = "ENERGY"
)

type Coord struct {
	X int
	Y int
}

type BuildingPrices struct {
	Defense int `json:"DEFENSE"`
	Attack  int `json:"ATTACK"`
	Energy  int `json:"ENERGY"`
}

var buildingPrice = map[string]int{
	"DEFENSE": 0,
	"ATTACK":  0,
	"ENERGY":  0,
}

var buildingCommandVal = map[string]int{
	"DEFENSE": 0,
	"ATTACK":  1,
	"ENERGY":  2,
}

type GameDetails struct {
	Round          int `json:"round"`
	MapWidth       int `json:"mapWidth"`
	MapHeight      int `json:"mapHeight"`
	BuildingPrices `json:"buildingPrices"`
}

type Player struct {
	PlayerType string `json:"playerType"`
	Energy     int    `json:"energy"`
	Health     int    `json:"health"`
}

type Building struct {
	X          int    `json:"x"`
	Y          int    `json:"y"`
	Health     int    `json:"health"`
	PlayerType string `json:"playerType"`
}

type Missile struct {
	X          int    `json:"x"`
	Y          int    `json:"y"`
	PlayerType string `json:"playerType"`
}

type Cell struct {
	X         int        `json:"x"`
	Y         int        `json:"y"`
	Buildings []Building `json:"buildings"`
	Missiles  []Missile  `json:"missiles"`
	CellOwner string     `json:"cellOwner"`
}

type GameState struct {
	GameDetails `json:"gameDetails"`
	Players     []Player `json:"players"`
	GameMap     [][]Cell `json:"gameMap"`
}

const stateFilename = "state.json"
const commandFilename = "command.txt"

var command string
var gameState GameState
var gameDetails GameDetails
var myself Player
var opponent Player
var gameMap [][]Cell
var missiles []Missile
var buildings []Building

func main() {
	runGameCycle()
	writeCommand()
}

func writeCommand() {
	err := ioutil.WriteFile(commandFilename, []byte(command), 0666)
	if err != nil {
		panic(err)
	}
}

func init() {
	rand.Seed(time.Now().Unix())

	data, err := ioutil.ReadFile(stateFilename)
	if err != nil {
		panic(err.Error())
	}

	var gameState GameState
	err = json.Unmarshal(data, &gameState)
	if err != nil {
		panic(err.Error())
	}

	// load some convenience variables
	gameDetails = gameState.GameDetails
	gameMap = gameState.GameMap
	buildingPrice[Attack] = gameDetails.BuildingPrices.Attack
	buildingPrice[Defense] = gameDetails.BuildingPrices.Defense
	buildingPrice[Energy] = gameDetails.BuildingPrices.Energy

	for _, player := range gameState.Players {
		switch player.PlayerType {
		case "A":
			myself = player
		case "B":
			opponent = player
		}
	}

	for x := 0; x < gameDetails.MapHeight; x++ {
		for y := 0; y < gameDetails.MapWidth; y++ {
			cell := gameMap[x][y]
			for missileIndex := 0; missileIndex < len(cell.Missiles); missileIndex++ {
				missiles = append(missiles, cell.Missiles[missileIndex])
			}
			for buildingIndex := 0; buildingIndex < len(cell.Buildings); buildingIndex++ {
				buildings = append(buildings, cell.Buildings[buildingIndex])
			}
		}
	}
}

func runGameCycle() {
	var row int
	var coord = Coord{-1, -1}

	if underAttack(&row) && canBuild(Defense) {
		coord = chooseLocationToDefend(row)
		buildBuilding(Defense, coord)
	} else if canBuild(Attack) {
		buildBuilding(Attack, coord)
	} else {
		doNothing()
	}
}

func underAttack(row *int) bool {
	*row = -1
	for _, missile := range missiles {
		if missile.PlayerType == opponent.PlayerType {
			*row = missile.Y
			break
		}
	}
	return *row >= 0
}

func chooseLocationToDefend(row int) Coord {
	var col = 0
	for _, building := range buildings {
		if building.PlayerType == myself.PlayerType && building.Y == row {
			if building.X > col {
				col = building.X
			}
		}
	}
	if col >= (gameDetails.MapWidth/2)-1 {
		return randomUnoccupiedCoordinate()
	}

	return Coord{X: col + 1, Y: row}
}

func canBuild(buildingType string) bool {
	return myself.Energy >= buildingPrice[buildingType]
}

func buildBuilding(buildingType string, coord Coord) {
	if coord.X < 0 || coord.Y < 0 {
		coord = randomUnoccupiedCoordinate()
	}
	command = fmt.Sprintf("%d,%d,%d", coord.X, coord.Y, buildingCommandVal[buildingType])
}

func doNothing() {
	command = ""
}

func randomCoordinate() Coord {
	var coord = Coord{}
	coord.X = rand.Intn(gameDetails.MapWidth / 2)
	coord.Y = rand.Intn(gameDetails.MapHeight)
	return coord
}

func randomUnoccupiedCoordinate() Coord {
	var coord Coord

	for {
		coord = randomCoordinate()
		if isOccupied(coord) == false {
			break
		}
	}
	return coord
}

func isOccupied(coord Coord) bool {
	if coord.X < 0 || coord.X >= gameDetails.MapWidth || coord.Y < 0 || coord.Y >= gameDetails.MapHeight {
		return false
	}
	var cell = gameMap[coord.X][coord.Y]
	return len(cell.Buildings) != 0
}

func prettyPrint(v interface{}) {
	b, _ := json.MarshalIndent(v, "", "  ")
	println(string(b))
}
