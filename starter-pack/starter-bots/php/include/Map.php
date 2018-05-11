<?php
class Map
{
  const DEFENSE_STR = 'DEFENSE';
  const ATTACK_STR = 'ATTACK';
  const ENERGY_STR = 'ENERGY';
  const DEFENSE = 0;
  const ATTACK = 1;
  const ENERGY = 2;
  
  protected $_map;
  
  public function __construct ($map)
  {
    $this->_map = $map;
  }
  
  /**
   * Returns the building at a set of coordinates or false if empty
   */
  public function getBuilding($x,$y)
  {
    return count($this->_map[$y][$x]->buildings) ? $this->_map[$y][$x]->buildings[0] : false;
  }
  
  /**
   * Returns the missiles at a set of coordinates or false if no missiles
   */
  public function getMissiles($x,$y)
  {
    return count($this->_map[$y][$x]->missiles) ? $this->_map[$y][$x]->missiles : false;
  }
  
  /**
   * Returns the x coordinate of the last empty cell in a row
   */
  public function getLastEmptyCell($y)
  {
    for ($x = count($this->_map[$y])/2 - 1; $x >= 0; $x--)
    {
      if (!$this->getBuilding($x,$y))
      {
        return $x;
      }
    }
    return false;
  }
  
  /**
   * Returns the x coordinate of the first empty cell in a row
   */
  public function getFirstEmptyCell($y)
  {
    for ($x = 0; $x < count($this->_map[$y])/2; $x++)
    {
      if (!$this->getBuilding($x,$y))
      {
        return $x;
      }
    }
    return false;
  }
  
  /**
   * Returns an array of all valid empty build cells
   */
  public function getValidBuildCells()
  {
    $emptyCells = [];
    foreach ($this->_map as $row)
    {
      foreach ($row as $cell)
      {
        if ($cell->cellOwner == 'A' && !count($cell->buildings))
        {
          $emptyCells[] = $cell;
        }
      }
    }
    
    return $emptyCells;
  }
  
  /**
   * Checks if a row is currently under attack by an enemy
   */
  public function isAttackedRow($y)
  {
    foreach ($this->_map[$y] as $cell)
    {
      foreach ($cell->missiles as $missile)
      {
        if ($missile->playerType == 'B')
        {
          return true;
        }
      }
    }
    return false;
  }
  
  /**
   * Checks if there is a friendly defensive building in a row
   */
  public function rowHasOwnDefense($y)
  {
    foreach ($this->_map[$y] as $cell)
    {
      foreach ($cell->buildings as $building)
      {
        if ($building->buildingType == self::DEFENSE_STR && $building->playerType == 'A')
        {
          return true;
        }
      }
    }
    return false;
  }
}
