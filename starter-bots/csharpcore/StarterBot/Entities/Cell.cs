using Newtonsoft.Json;
using StarterBot.Enums;

namespace StarterBot.Entities
{
    public class Cell
    {
        public int X { get; set; }
        public int Y { get; set; }
        public PlayerType PlayerType { get; set; }
    }
}