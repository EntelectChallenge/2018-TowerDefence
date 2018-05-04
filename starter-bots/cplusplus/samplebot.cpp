#include <iostream> //not needed but useful for debugging
#include <vector> 
#include <fstream>
#include <stdlib.h>  //for random number generation
#include <time.h> //seeding random number generation
using namespace std;

//maximum size for game state - Unknown
const int MAX_MAP_WIDTH = 100;
const int MAX_MAP_HEIGHT = 100;

class BuildingInfo
{
    public:
    string type;
    int energyCost;
    
    //other possible info
    int constructionTime;
    int health;
    int weaponCooldownTime;
    int weaponDamage;
    int energyGenerated;
    
    BuildingInfo(){}
    
    BuildingInfo(string in_type,int in_energyCost)
    {
        type = in_type;
        energyCost = in_energyCost;
    }
};

class Building
{
    public:
    char buildingType;
    char owner;
    int constructionTimeLeft;
    int health;
    int weaponCooldownTimeLeft;
    int weaponDamage;
    int energyGeneratedPerTurn;
    
    Building()
    {
        buildingType = 'N';
    }
};

class Missile
{
    public:
    char owner;
    int damage;
    
    Missile()
    {}
    
    Missile(char in_owner,int in_damage)
    {
        owner = in_owner;
        damage = in_damage;
    }
    
};

class Player
{
    public:
    char owner;
    int energy;
    int health;
    int hitsTaken;
    int score;
    
    Player()
    {}
};

class Cell
{
    public:
    char owner;
    Building building;
    vector<Missile> missile;
    
    Cell(){}
};

class Gamestate
{
    public:
    int roundNumber;
    int mapWidth;
    int mapHeight;
    
    vector<BuildingInfo> buildingInfo;
    
    Player player[2];
    
    Cell cell[MAX_MAP_WIDTH][MAX_MAP_HEIGHT];
    
    //unknown values
    int baseEnergyGenerated;
    int maxRounds;
    
    Gamestate()
    {
        //setting guessed values
        baseEnergyGenerated = 5;
        maxRounds = 1000;
    }
    
    //reads gamestate from TextMap.txt
    void readinText()
    {
        string inputline;
        ifstream statefile ("textMap.txt");
        
        while ( getline(statefile,inputline) )
        {
            //start of game info input
            if(inputline == "XXXXXXXXX GAME INFO XXXXXXXXX") 
            {
                getline(statefile,inputline);
                roundNumber = stoi(inputline.substr(15));
                
                getline(statefile,inputline);
                mapWidth = stoi(inputline.substr(12));
                
                getline(statefile,inputline);
                mapHeight = stoi(inputline.substr(13));
                
                getline(statefile,inputline);
            }
            
            //start of building info input
            if(inputline == "****** BUILDING PRICES ******")
            {
                getline(statefile,inputline);
                
                while(inputline != "*****************************")
                {
                    string building_type = inputline.substr(0,inputline.find(":")-1);
                    int building_energyCost = stoi(inputline.substr(inputline.find(":")+2));
                    
                    buildingInfo.push_back(BuildingInfo(building_type,building_energyCost));
                    
                    getline(statefile,inputline);
                }
            }
            
            //start of player A inputs
            if(inputline == "---------- PLAYER A ----------")
            {
                player[0].owner = 'A';
                
                getline(statefile,inputline);
                player[0].energy = stoi(inputline.substr(9));
                
                getline(statefile,inputline);
                player[0].health = stoi(inputline.substr(9));
                
                getline(statefile,inputline);
                player[0].hitsTaken = stoi(inputline.substr(12));
                
                getline(statefile,inputline);
                player[0].score = stoi(inputline.substr(8));
                
                getline(statefile,inputline);
            }
            
            //start of player B inputs
            if(inputline == "---------- PLAYER B ----------")
            {
                player[1].owner = 'B';
                
                getline(statefile,inputline);
                player[1].energy = stoi(inputline.substr(9));
                
                getline(statefile,inputline);
                player[1].health = stoi(inputline.substr(9));
                
                getline(statefile,inputline);
                player[1].hitsTaken = stoi(inputline.substr(12));
                
                getline(statefile,inputline);
                player[1].score = stoi(inputline.substr(8));
                
                getline(statefile,inputline);
            }
            
            //start of map input
            if(inputline == "############# MAP #############")
            {
                for(int map_y = 0;map_y < mapHeight;map_y++)
                {
                    getline(statefile,inputline);
                    for(int map_x = 0;map_x < mapWidth;map_x++)
                    {
                        cell[map_x][map_y].building.buildingType = inputline[map_x*11 + 5];
                        cell[map_x][map_y].owner = (map_x * 2 < mapWidth ? 'A' : 'B');
                    }
                }
            }
            
            //start of building data
            if(inputline == "######## BUILDING DATA #########")
            {
                getline(statefile,inputline);//FORMAT...
                getline(statefile,inputline);//blank line
                getline(statefile,inputline);//first building info
                while(inputline != "###############################")
                {
                    inputline = inputline.substr(inputline.find("[")+1);
                    int map_x = stoi(inputline.substr(0,inputline.find(",")));
                    inputline = inputline.substr(inputline.find(",")+1);
                    int map_y = stoi(inputline.substr(0,inputline.find("]")));
                    inputline = inputline.substr(inputline.find(" ")+1);
                    
                    cell[map_x][map_y].building.owner = inputline[0];
                    inputline = inputline.substr(inputline.find("|")+1);
                    
                    cell[map_x][map_y].building.constructionTimeLeft = stoi(inputline.substr(0,inputline.find("|")));
                    inputline = inputline.substr(inputline.find("|")+1);
                    
                    cell[map_x][map_y].building.health = stoi(inputline.substr(0,inputline.find("|")));
                    inputline = inputline.substr(inputline.find("|")+1);
                    
                    cell[map_x][map_y].building.weaponCooldownTimeLeft = stoi(inputline.substr(0,inputline.find("|")));
                    inputline = inputline.substr(inputline.find("|")+1);
                    
                    cell[map_x][map_y].building.weaponDamage = stoi(inputline.substr(0,inputline.find("|")));
                    inputline = inputline.substr(inputline.find("|")+1);
                    
                    cell[map_x][map_y].building.energyGeneratedPerTurn = stoi(inputline);
                    
                    getline(statefile,inputline);
                }
            }
            
            //start of missile data
            if(inputline == "####### MISSILE DATA ########")
            {
                getline(statefile,inputline);//FORMAT...
                getline(statefile,inputline);//blank line
                getline(statefile,inputline);//first missile info
                while(inputline != "###############################")
                {
                    inputline = inputline.substr(inputline.find("[")+1);
                    int map_x = stoi(inputline.substr(0,inputline.find(",")));
                    inputline = inputline.substr(inputline.find(",")+1);
                    int map_y = stoi(inputline.substr(0,inputline.find("]")));
                    inputline = inputline.substr(inputline.find(" ")+1);
                    
                    char missile_owner = inputline[0];
                    inputline = inputline.substr(inputline.find("|")+1);
                    int missile_damage = stoi(inputline);
                    
                    cell[map_x][map_y].missile.push_back(Missile(missile_owner,missile_damage));
                    
                    getline(statefile,inputline);
                }
            }
        }
        
        statefile.close();
    }
    
    vector<string> possible_moves()
    {
        vector<string> moves;
        moves.push_back("");
        
        for(int x = 0;x<mapWidth;x++)
        {
            for(int y = 0;y<mapHeight;y++)
            {
                for(unsigned int building_type = 0;building_type < buildingInfo.size();building_type++)
                {
                    if(cell[x][y].owner == 'A' and 
                       cell[x][y].building.buildingType =='N' and
                       player[0].energy >= buildingInfo[building_type].energyCost)
                    {
                        moves.push_back(to_string(x)+","+to_string(y)+","+to_string(building_type));
                    }
                }
            }
        }
        
        return moves;
    }
};

int main()
{
    //initialize random seed
    srand (time(NULL));
    
    //create gamestate object
    Gamestate gamestate;
    gamestate.readinText();
    
    //get legal moves rom gamestate
    vector<string> possible_moves = gamestate.possible_moves();
    
    //write random move to command file
    ofstream movefile ("command.txt");
    movefile << possible_moves[rand()%possible_moves.size()];
    movefile.close();
    
    return 0;
}