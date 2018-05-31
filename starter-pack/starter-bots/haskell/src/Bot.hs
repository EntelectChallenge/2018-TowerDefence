module Bot
  where

import Interpretor (GameState(..),
                    Command,
                    GameDetails(..),
                    Building(..),
                    CellStateContainer(..),
                    PlayerType(..),
                    BuildingType(..),
                    BuildingPriceIndex(..),
                    Player(..))
import Data.List
import System.Random
import Control.Monad

-- Predicate combination operator
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) f g = \ input -> f input && g input

cellBelongsTo :: PlayerType -> CellStateContainer -> Bool
cellBelongsTo typeOfPlayer =
  (==typeOfPlayer) . cellOwner

cellContainsBuildingType :: BuildingType -> CellStateContainer -> Bool
cellContainsBuildingType typeOfBuilding =
  any ((==typeOfBuilding) . buildingType) . buildings

enemyHasAttacking :: GameState -> Int -> Bool
enemyHasAttacking state =
  any cellContainsEnemyAttacker . ((gameMap state) !!)
  where
    cellContainsEnemyAttacker =
      (cellBelongsTo B) &&& (cellContainsBuildingType ATTACK)

cellBelongsToMe :: CellStateContainer -> Bool
cellBelongsToMe = cellBelongsTo A

iDontHaveDefense :: GameState -> Int -> Bool
iDontHaveDefense state =
  not . any cellContainDefenseFromMe . ((gameMap state) !!)
  where
    cellContainDefenseFromMe =
      cellBelongsToMe &&& (cellContainsBuildingType DEFENSE)

thereIsAnEmptyCellInRow :: GameState -> Int -> Bool
thereIsAnEmptyCellInRow (GameState {gameMap = gameMap'})=
  any cellIsEmpty . (gameMap' !!)

indexOfFirstEmpty :: GameState -> Int -> Maybe Int
indexOfFirstEmpty (GameState {gameMap = gameMap'}) =
  fmap yPos . find (cellIsEmpty &&& cellBelongsToMe) . (gameMap' !!)

defendAttack :: GameState -> Maybe (Int, Int, BuildingType)
defendAttack state@(GameState _ _ (GameDetails _ _ _ height _ _)) = do
  x <- find rowUnderAttack [0..height - 1]
  y <- indexOfFirstEmpty state x
  return (x, y, DEFENSE)
  where
    rowUnderAttack = (enemyHasAttacking state) &&&
                     (iDontHaveDefense state) &&&
                     (thereIsAnEmptyCellInRow state)

hasEnoughEnergyForMostExpensiveBuilding :: GameState -> Bool
hasEnoughEnergyForMostExpensiveBuilding state@(GameState _ _ (GameDetails { buildingPrices = prices })) =
  ourEnergy >= maxPrice
  where
    ourEnergy = energy ourPlayer
    ourPlayer = (head . filter ((==A) . playerType) . players) state
    maxPrice = maximum towerPrices
    towerPrices = map ($ prices) [attackTowerCost, defenseTowerCost, energyTowerCost]

cellIsEmpty :: CellStateContainer -> Bool
cellIsEmpty = ([] ==) . buildings

myEmptyCells :: [[CellStateContainer]] -> [CellStateContainer]
myEmptyCells =
  concat . map (filter isMineAndIsEmpty)
  where
      isMineAndIsEmpty = cellIsEmpty &&& cellBelongsToMe

randomEmptyCell :: RandomGen g => g -> GameState -> ((Int, Int), g)
randomEmptyCell gen (GameState {gameMap = mapGrid}) =
  let emptyCells                = myEmptyCells mapGrid
      (randomInt, newGenerator) = next gen
      emptyCell                 = emptyCells !! mod randomInt (length emptyCells)
  in ((xPos emptyCell, yPos emptyCell), newGenerator)

randomBuilding :: RandomGen g => g -> (BuildingType, g)
randomBuilding gen =
  let (randomInt, gen') = next gen
      buildingIndex     = mod randomInt 3
  in (case buildingIndex of
        0 -> DEFENSE
        1 -> ATTACK
        _ -> ENERGY,
      gen')

buildRandomly :: RandomGen g => g -> GameState -> Maybe (Int, Int, BuildingType)
buildRandomly gen state =
  if not $ hasEnoughEnergyForMostExpensiveBuilding state
  then Nothing
  else let ((x, y),   gen') = randomEmptyCell gen  state
           (building, _)    = randomBuilding gen'
       in Just (x, y, building)

doNothingCommand :: Command
doNothingCommand = ""

build :: Int -> Int -> BuildingType -> Command
build x y buildingType' =
  show x ++ "," ++ show y ++ "," ++
  case buildingType' of
    DEFENSE -> "0"
    ATTACK  -> "1"
    ENERGY  -> "2"

decide :: RandomGen g => g -> GameState -> Command
decide gen state =
  case msum [defendAttack state, buildRandomly gen state] of
    Just (x, y, building) -> build x y building
    Nothing               -> doNothingCommand
