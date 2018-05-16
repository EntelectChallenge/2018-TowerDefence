program StarterBot_DelphiXE;
{$APPTYPE CONSOLE}

uses
  SysUtils,
  JSON in 'core\JSON.pas';

const
  STATUS_FILENAME = 'state.json';
  COMMAND_FILENAME = 'command.txt';

type
  TValueArray = array of TJSONValue;

procedure WriteCommand(ACommandText: string);
var
  F: Text;
begin
  Assign(F, COMMAND_FILENAME);
  try
    Rewrite(F);
    Writeln(F, ACommandText);
  finally
    CloseFile(F);
  end;
end;

function ReadJSONFile(AFileName: string): TJSONValue;
var
  F: Text;
  TextLn: string;
  JSONStr: string;
begin
  JSONStr := '';
  Assign(F, AFileName);
  try
    Reset(F);
    while not Eof(F) do
    begin
      Readln(F, TextLn);
      JSONStr := JSONStr + TextLn + #10;
    end;
    Result :=  TJSONValue.Decompose(JSONStr);
  finally
    CloseFile(F);
  end;
end;

function ExtractMapDimentions(AGameStatus: TJSONValue; var AMapWidth, AMapHeight: Integer): Boolean;
var
  GameDetails: TJSONValue;
  mapWidth: TJSONValue;
  mapHeight: TJSONValue;
begin
  if not Assigned(AGameStatus) then 
    Result := False
  else if not AGameStatus.FindMemberValue('gameDetails', GameDetails) then
    Result := False
  else if not GameDetails.FindMemberValue('mapWidth', mapWidth) then
    Result := False
  else if not GameDetails.FindMemberValue('mapHeight', mapHeight) then
    Result := False
  else
  begin
    AMapWidth := mapWidth.ValueAsInteger;
    AMapHeight := mapHeight.ValueAsInteger;
    Result := True;  
  end;
end;

function ExtractCellInfo(AGameMapCell: TJSONValue; var ACellX, ACellY: Integer;
  var AIsCellEmpty, AIsCellOwner: Boolean): Boolean;
var
  CellX, CellY: TJSONValue;   
  Buildings: TJSONValue;   
  CellOwner: TJSONValue;   
begin
  if not Assigned(AGameMapCell) then
    Result := False
  else if not AGameMapCell.FindMemberValue('x', CellX) then
    Result := False
  else if not AGameMapCell.FindMemberValue('y', CellY) then
    Result := False
  else if not AGameMapCell.FindMemberValue('buildings', Buildings) then
    Result := False
  else if not AGameMapCell.FindMemberValue('cellOwner', CellOwner) then
    Result := False
  else 
  begin  
    ACellX := CellX.ValueAsInteger;
    ACellY := CellY.ValueAsInteger;
    AIsCellEmpty := Buildings.Count = 0;
    AIsCellOwner := CellOwner.ValueAsString = 'A';
    Result := True;
  end;       
end;
  
function ExtractEmptyCellList(AGameStatus: TJSONValue; var ACellCount: Integer; 
  var AEmptyCellList: TValueArray): Boolean;
var
  GameMap: TJSONValue;
  GameMapRow: TJSONValue;
  GameMapCol: TJSONValue;

  CellIndex: Integer;
  MapWidth: Integer;
  MapHeight: Integer;
  IndexX, IndexY: Integer;

  CellX, CellY: Integer;
  IsCellEmpty: Boolean;
  IsCellOwner: Boolean;
  
begin
  if not Assigned(AGameStatus) then
    Result := False
  else if not ExtractMapDimentions(AGameStatus, MapWidth, MapHeight) then
    Result := False
  else if not AGameStatus.FindMemberValue('gameMap', GameMap) then
    Result := False
  else
  begin
    CellIndex := 0;
    ACellCount := MapHeight * MapWidth div 2;
    SetLength(AEmptyCellList, ACellCount);
    for IndexY := 0 to MapHeight-1 do
    begin
      if IndexY < GameMap.Count then
      begin
        GameMapRow := GameMap.Item[IndexY];
        for IndexX := 0 to MapWidth-1 do
        begin
          if IndexX < GameMapRow.Count then
          begin
            GameMapCol := GameMapRow.Item[IndexX];
            if ExtractCellInfo(GameMapCol, CellX, CellY, IsCellEmpty, IsCellOwner) and
              (IsCellEmpty and IsCellOwner) then
            begin
              AEmptyCellList[CellIndex] := GameMapCol;
              Inc(CellIndex);
            end;
          end;
        end;
      end;
    end;
    ACellCount := CellIndex;
    SetLength(AEmptyCellList, ACellCount);
    Result := True;
  end;
end;

function ExtractBuildingsPrices(AGameStatus: TJSONValue; var ABuildingPrices: TJSONValue): Boolean;
var
  GameDetails: TJSONValue;
begin
  if not Assigned(AGameStatus) then
    Result := False
  else if not AGameStatus.FindMemberValue('gameDetails', GameDetails) then
    Result := False
  else if not GameDetails.FindMemberValue('buildingPrices', ABuildingPrices) then
    Result := False
  else
    Result := True;
end;

function IsPlayerType(APlayer: TJSONValue; APlayerType: string): Boolean;
var
  PlayerType: TJSONValue;
begin
  if not Assigned(APlayer) then
    Result := False
  else if not APlayer.FindMemberValue('playerType', PlayerType) then
    Result := False
  else
    Result := APlayerType = PlayerType.ValueAsString;
end;

function ExtractPlayerInfo(AGameStatus: TJSONValue; APlayerType: string; 
  var APlayerInfo: TJSONValue): Boolean;
var
  Players: TJSONValue;
  Index: Integer;
begin
  if not Assigned(AGameStatus) then
    Result := False
  else if not AGameStatus.FindMemberValue('players', Players) then
    Result := False
  else 
  begin
    Index := 0;
    repeat
      if Index < Players.Count then
        APlayerInfo := Players.Item[Index] else
        APlayerInfo := nil;
      Inc(Index);
    until (IsPlayerType(APlayerInfo, APlayerType) or not Assigned(APlayerInfo));
    Result := Assigned(APlayerInfo);
  end;
end;

procedure ChooseRandomBuildCell(AEmptyCellList: TValueArray; var ABuildCellX, ABuildCellY: Integer);
var
  RNDIndex: Integer;
  EmptyCell: TJSONValue;
  IsCellEmpty: Boolean;
  IsCellOwner: Boolean;
begin
  RNDIndex := Random(Length(AEmptyCellList));
  EmptyCell := AEmptyCellList[RNDIndex];
  ExtractCellInfo(EmptyCell, ABuildCellX, ABuildCellY, IsCellEmpty, IsCellOwner);
end;

procedure ChooseRandomBuilding(ABuildingPrices: TJSONValue; var ABuildingType, ABuildingCost: Integer);
begin                                     
  ABuildingType := Random(3);
  case ABuildingType of
    0: ABuildingCost := ABuildingPrices.Member['DEFENSE'].ValueAsInteger;
    1: ABuildingCost := ABuildingPrices.Member['ATTACK'].ValueAsInteger;
    2: ABuildingCost := ABuildingPrices.Member['ENERGY'].ValueAsInteger; 
  end;
end;

function CanAffordBuilding(APlayerInfo: TJSONValue; ABuildingCost: Integer): Boolean;
var
  Energy: TJSONValue;
begin
  if not Assigned(APlayerInfo) then
    Result := False
  else if not APlayerInfo.FindMemberValue('energy', Energy) then
    Result := False
  else 
    Result := ABuildingCost <= Energy.ValueAsInteger;
end;

function ExecuteRound: string;
var
  GameStatus: TJSONValue;
  EmptyCellCount: Integer;
  EmptyCellList: TValueArray;
  BuildingType: Integer;
  BuildCellX, BuildCellY: Integer;
  BuildingPrices: TJSONValue;
  PlayerInfo: TJSONValue;
  BuildingCost: Integer;
begin
  GameStatus := ReadJSONFile(STATUS_FILENAME);
  try
    if not Assigned(GameStatus) then
      Result := 'No Command' 
    else if not ExtractEmptyCellList(GameStatus, EmptyCellCount, EmptyCellList) then
      Result := 'No Command'
    else if not ExtractBuildingsPrices(GameStatus, BuildingPrices) then
      Result := 'No Command'
    else if not ExtractPlayerInfo(GameStatus, 'A', PlayerInfo) then
      Result := 'No Command'
    else
    begin
      ChooseRandomBuildCell(EmptyCellList, BuildCellX, BuildCellY);
      ChooseRandomBuilding(BuildingPrices, BuildingType, BuildingCost);
      if not CanAffordBuilding(PlayerInfo, BuildingCost) then
        Result := 'No Command' else
        Result := 
          IntToStr(BuildCellX) +','+
          IntToStr(BuildCellY) +','+
          IntToStr(BuildingType);    
    end;
  finally
    GameStatus.Free;
  end;
end;

begin
  Randomize;
  try
    WriteCommand(ExecuteRound);
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.

