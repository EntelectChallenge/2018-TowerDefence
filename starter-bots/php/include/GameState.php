<?php
require_once('Map.php');

class GameState
{
  protected $_state;
  protected $_map;
  
  public function __construct ($filename)
  {
    $this->_state = json_decode(file_get_contents($filename));
    $_map = null;
  }
  
  /**
   * Returns the entire state object for manual processing
   */
  public function getState()
  {
    return $this->_state;
  }
  
  public function getMapWidth()
  {
    return $this->_state->gameDetails->mapWidth;
  }
  
  public function getMapHeight()
  {
    return $this->_state->gameDetails->mapHeight;
  }
  
  public function getPlayerA()
  {
    foreach ($this->_state->players as $player)
    {
      if ($player->playerType == "A")
      {
        return $player;
      }
    }
  }

  public function getPlayerB()
  {
    foreach ($this->_state->players as $player)
    {
      if ($player->playerType == "B")
      {
        return $player;
      }
    }
  }
  
  /**
   * Looks up the price of a particular building type
   */
  public function getBuildingPrice(int $type)
  {
    switch ($type)
    {
      case Map::DEFENSE:
        $str = MAP::DEFENSE_STR;
        break;
      case Map::ATTACK:
        $str = MAP::ATTACK_STR;
        break;
      case Map::ENERGY:
        $str = MAP::ENERGY_STR;
        break;
      default:
        return false;
      break;
    }
    return $this->_state->gameDetails->buildingPrices->$str;
  }
  
  /**
   * Returns the current round number
   */
  public function getRound()
  {
    return $this->_state->gameDetails->round();
  }
  
  /**
   * Returns a Map object for examining the playing field
   */
  public function getMap()
  {
    if ($this->_map === null)
    {
      $this->_map = new Map($this->_state->gameMap);
    }
    
    return $this->_map;
  }
}
