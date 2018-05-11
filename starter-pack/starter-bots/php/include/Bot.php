<?php
class Bot
{  
  protected $_game;
  protected $_map;
  
  public function __construct (GameState $state)
  {
    $this->_game = $state;
    $this->_map = $this->_game->getMap();
  }
  
  /**
   * This is the main function for deciding which action to take
   *
   * Returns a valid action string
   */
  public function decideAction()
  {
    //Check if we should defend
    list($x,$y,$building) = $this->checkDefense();   
  
    //If no defend orders then build randomly
    list($x,$y,$building) = $x === null ? $this->buildRandom() : [$x, $y, $building];
    
    if ($x !== null && $this->_game->getBuildingPrice($building) <= $this->_game->getPlayerA()->energy)
    {
      return "$x,$y,$building";
    }
    return "";
  }
  
  /**
   * Checks if a row is being attacked and returns a build order if there is an empty space
   * and no defensive buildings in the that row.
   */
  protected function checkDefense()
  {
    for ($row = 0; $row < $this->_game->getMapHeight(); $row++)
    {
      if ($this->_map->isAttackedRow($row) && !$this->_map->rowHasOwnDefense($row))
      {
        list($x,$y,$building) = $this->buildDefense($row);
        if ($x !== null)
        {
          return [$x,$y,$building];
        }
      }
    }
    return [null, null, null];
  }
  
  /**
   * Returns defensive build order at last empty cell in a row
   */
  protected function buildDefense($row)
  {
    //Check for last valid empty cell
    $x = $this->_map->getLastEmptyCell($row);
    return $x === false ? [$x, $y, Map::DEFENSE] : [null, null, null];
  }
  
  /**
   * Returns a random build order on an empty cell
   */
  protected function buildRandom()
  {
    $emptyCells = $this->_map->getValidBuildCells();
    if (!count($emptyCells))
    {
      return [null, null, null];
    }
    
    $cell = $emptyCells[rand(0,count($emptyCells)-1)];
    $building = rand(0,2);
    
    return [$cell->x,$cell->y,$building];
  }
}
