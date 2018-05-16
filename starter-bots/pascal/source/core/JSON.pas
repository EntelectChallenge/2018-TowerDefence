unit JSON;

interface

uses
  // Constants, Types & Utils
  // Non-Visual Dependencies
  // Visual Dependencies
  // Custom Compiler Dependencies
  // Automated Compiler Dependencies
  System.Classes, System.SysUtils, System.StrUtils, System.Variants,
  System.TypInfo, System.RTTI;

type
  EJSONError = class(Exception);
  EJSONParseError = class(EJSONError);
  EJSONConvertError = class(EJSONError);

const
  CIndentSize = 2;

type
  { TJSONType }
  TJSONType = (
    jtUnknown,    // Unknown Error Value
    jtNull,       // NULL Value: "null"
    jtBoolean,    // Boolean Value: "true" or "false"
    jtNumber,     // Numeric Value: 0..9
    jtString,     // String: Value: "..."
    jtArray,      // Array Value: [...]
    jtObject);    // Object Value: {...}
function JSONTypeToStr(AType: TJSONType): string;

type
  TJSONValue = class;

  { TJSONCore }
  /// <summary>
  ///   The JSONCore Class forms the basis for this JSON implementation.
  /// </summary>
  TJSONCore = class abstract(TObject)
  // Public Declarations.
  public
    /// <summary>
    ///   Returns the maximum estimated Text Buffer Size required to store the
    ///   JSON Object. The actual size might be smaller.
    /// </summary>
    ///
    /// <returns>Integer - The size required for a Text Buffer</returns>
    function EstimatedSize(AIndent: Integer = -1): Integer; virtual; abstract;

    /// <summary>
    ///   Composes the JSON Object into JSON Text.
    /// </summary>
    ///
    /// <param name="AIndent (Default = -1)">
    ///   Integer - Indentation Value. Spesifying values smaller that zero will
    ///   suppress indentation, producing the most optimum JSON Text.
    /// </param>
    ///
    /// <remarks>
    ///   The resulting JSON Text can be formatted by spesifying a positive
    ///   indentation value.
    /// </remarks>
    ///
    /// <returns>String - JSON Data Text</returns>
    function Compose(AIndent: Integer = -1): string; virtual; abstract;

    /// <summary>
    ///   Decomposes (parse) the JSON Text into a JSON Object.
    /// </summary>
    ///
    /// <param name="ADataStr">
    ///   String - Data String containing the JSON Data that has to be parsed.
    /// </param>
    ///
    /// <returns>TJSONValue - The decomposed JSON Object</returns>
    class function Decompose(const ADataStr: string): TJSONValue; overload; virtual; abstract;

    /// <summary>
    ///   Decomposes (parse) the JSON Text into a JSON Object.
    /// </summary>
    ///
    /// <param name="ADataStr">
    ///   String - Data String containing the JSON Data that has to be parsed.
    /// </param>
    ///
    /// <param name="AIndex">
    ///   Integer - This value indicates the zero-based Index where parsing
    ///   has to start. The changed value will be returned to the calling
    ///   routine, indicating how many characters has been consumed.
    /// </param>
    ///
    /// <returns>TJSONValue - The decomposed JSON Object</returns>
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; overload; virtual; abstract;

  end;

  { TJSONCoreArray }
  TJSONCoreArray = TArray<TJSONCore>;

  { TJSONValue }
  /// <summary>
  ///   The JSONValue Class forms the basis for all the JSON Value Types.
  /// </summary>
  TJSONValue = class(TJSONCore)
  private
  // Protected Declarations.
  protected
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual;
    function GetItem(AIndex: Integer): TJSONValue; virtual;
    function GetMember(AName: string): TJSONValue; virtual;
    function GetMemberName(AIndex: Integer): string; virtual;
    function GetValue: Variant; virtual;
    function GetValueAsBoolean: Boolean; virtual;
    function GetValueAsDateTime: TDateTime; virtual;
    function GetValueAsFloat: Extended; virtual;
    function GetValueAsInteger: Int64; virtual;
    function GetValueAsString: string; virtual;
    procedure SetCapacity(const ANewCapacity: Integer); virtual;
    procedure SetItem(AIndex: Integer; const AValue: TJSONValue); virtual;
    procedure SetMember(AName: string; const AValue: TJSONValue); virtual;
    procedure SetValue(const AValue: Variant); virtual;
    procedure SetValueAsBoolean(const AValue: Boolean); virtual;
    procedure SetValueAsDateTime(const AValue: TDateTime); virtual;
    procedure SetValueAsFloat(const AValue: Extended); virtual;
    procedure SetValueAsInteger(const AValue: Int64); virtual;
    procedure SetValueAsString(const AValue: string); virtual;

  // Public Declarations.
  public
    constructor Create;overload; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;

    /// <summary>
    ///   Returns the maximum estimated Text Buffer Size required to store the
    ///   JSON Object. The actual size might be smaller.
    /// </summary>
    ///
    /// <returns>Integer - The size required for a Text Buffer</returns>
    function EstimatedSize(AIndent: Integer = -1): Integer; override;

    /// <summary>
    ///   Composes the JSON Object into JSON Text.
    /// </summary>
    ///
    /// <param name="AIndent (Default = -1)">
    ///   Integer - Indentation Value. Spesifying values smaller that zero will
    ///   suppress indentation, producing the most optimum JSON Text.
    /// </param>
    ///
    /// <remarks>
    ///   The resulting JSON Text can be formatted by spesifying a positive
    ///   indentation value.
    /// </remarks>
    ///
    /// <returns>String - JSON Data Text</returns>
    function Compose(AIndent: Integer = -1): string; override;

    /// <summary>
    ///   Decomposes (parse) the JSON Text into a JSON Object.
    /// </summary>
    ///
    /// <param name="ADataStr">
    ///   String - Data String containing the JSON Data that has to be parsed.
    /// </param>
    ///
    /// <returns>TJSONValue - The decomposed JSON Object</returns>
    class function Decompose(const ADataStr: string): TJSONValue; override;

    /// <summary>
    ///   Decomposes (parse) the JSON Text into a JSON Object.
    /// </summary>
    ///
    /// <param name="ADataStr">
    ///   String - Data String containing the JSON Data that has to be parsed.
    /// </param>
    ///
    /// <param name="AIndex">
    ///   Integer - This value indicates the zero-based Index where parsing
    ///   has to start. The changed value will be returned to the calling
    ///   routine, indicating how many characters has been consumed.
    /// </param>
    ///
    /// <returns>TJSONValue - The decomposed JSON Object</returns>
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; override;

    class function ValueType: TJSONType; virtual;
    function FindMemberValue(AName: string; var AValue: TJSONValue): Boolean; virtual;
    procedure Clear; virtual;

    // These properties references the Data Value.
    property Value: Variant read GetValue write SetValue;
    property ValueAsBoolean: Boolean read GetValueAsBoolean write SetValueAsBoolean;
    property ValueAsInteger: Int64 read GetValueAsInteger write SetValueAsInteger;
    property ValueAsFloat: Extended read GetValueAsFloat write SetValueAsFloat;
    property ValueAsDateTime: TDateTime read GetValueAsDateTime write SetValueAsDateTime;
    property ValueAsString: string read GetValueAsString write SetValueAsString;
    // These properties references the Values in a List.
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property Item[AIndex: Integer]: TJSONValue read GetItem
      write SetItem; default;
    // These properties references the MemberNames of an Object.
    property MemberName[AIndex: Integer]: string read GetMemberName;
    // These properties references the Members of an Object.
    property Member[AName: string]: TJSONValue read GetMember
      write SetMember;

  end;

  { TJSONValueClass }
  TJSONValueClass = class of TJSONValue;

type
  { TJSONNull }
  /// <summary>
  ///   The JSONNull Class implements the functionality for a JSON Null Value.
  /// </summary>
  TJSONNull = class(TJSONValue)
  // Protected Declarations.
  protected
    function GetValue: Variant; override;

  // Public Declarations.
  public
    constructor Create; override;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    function Compose(AIndent: Integer = -1): string; override;
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; overload; override;
    class function ValueType: TJSONType; override;

  end;

type
  { TJSONBoolean }
  /// <summary>
  ///   The JSONNull Class implements the functionality for a JSON Boolean Value.
  /// </summary>
  TJSONBoolean = class(TJSONValue)
  // Private Declarations.
  private
    FValue: Boolean;

  // Protected Declarations.
  protected
    function GetValue: Variant; override;
    function GetValueAsBoolean: Boolean; override;
    function GetValueAsFloat: Extended; override;
    function GetValueAsInteger: Int64; override;
    function GetValueAsString: string; override;
    procedure SetValue(const AValue: Variant); override;
    procedure SetValueAsBoolean(const AValue: Boolean); override;
    procedure SetValueAsFloat(const AValue: Extended); override;
    procedure SetValueAsInteger(const AValue: Int64); override;
    procedure SetValueAsString(const AValue: string); override;

  // Public Declarations.
  public
    constructor Create; override;
    constructor Create(const AValue: Boolean); overload;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    function Compose(AIndent: Integer = -1): string; override;
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; overload; override;
    class function ValueType: TJSONType; override;
    procedure Clear; override;

  end;

type
  { TJSONNumberType }
  TJSONNumberType = (ntNone, ntInteger, ntFloat, ntDateTime);
function ExtractNumberType(ADataStr: string): TJSONNumberType; overload;
function ExtractNumberType(ADataStr: string; var AIndex: Integer): TJSONNumberType; overload;

type
  { TJSONNumber }
  /// <summary>
  ///   The JSONNull Class implements the functionality for a JSON Numeric Value.
  /// </summary>
  TJSONNumber = class(TJSONValue)
  // Private Declarations.
  private
    FIntValue: Int64;
    FFloatValue: Extended;
    FNumberType: TJSONNumberType;

  // Protected Declarations.
  protected
    function GetValue: Variant; override;
    function GetValueAsBoolean: Boolean; override;
    function GetValueAsDateTime: TDateTime; override;
    function GetValueAsFloat: Extended; override;
    function GetValueAsInteger: Int64; override;
    function GetValueAsString: string; override;
    procedure SetValue(const AValue: Variant); override;
    procedure SetValueAsBoolean(const AValue: Boolean); override;
    procedure SetValueAsDateTime(const AValue: TDateTime); override;
    procedure SetValueAsFloat(const AValue: Extended); override;
    procedure SetValueAsInteger(const AValue: Int64); override;
    procedure SetValueAsString(const AValue: string); override;

  // Public Declarations.
  public
    constructor Create; override;
    constructor Create(const AValue: Int64); overload;
    constructor Create(const AValue: Extended); overload;
    constructor Create(const AValue: TDateTime); overload;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    function Compose(AIndent: Integer = -1): string; override;
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; overload; override;
    class function ValueType: TJSONType; override;
    procedure Clear; override;

    // This property references the Type of the Number Stored.
    property NumberType: TJSONNumberType read FNumberType;

  end;

type
  { TJSONString }
  /// <summary>
  ///   The JSONNull Class implements the functionality for a JSON String Value.
  /// </summary>
  TJSONString = class(TJSONValue)
  // Private Declarations.
  private
    FValue: string;

  // Protected Declarations.
  protected
    function GetValue: Variant; override;
    function GetValueAsBoolean: Boolean; override;
    function GetValueAsDateTime: TDateTime; override;
    function GetValueAsFloat: Extended; override;
    function GetValueAsInteger: Int64; override;
    function GetValueAsString: string; override;
    procedure SetValue(const AValue: Variant); override;
    procedure SetValueAsBoolean(const AValue: Boolean); override;
    procedure SetValueAsDateTime(const AValue: TDateTime); override;
    procedure SetValueAsFloat(const AValue: Extended); override;
    procedure SetValueAsInteger(const AValue: Int64); override;
    procedure SetValueAsString(const AValue: string); override;

  // Public Declarations.
  public
    constructor Create; override;
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: AnsiString); overload;
    constructor Create(const AValue: WideString); overload;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    function Compose(AIndent: Integer = -1): string; override;
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; overload; override;
    class function ValueType: TJSONType; override;
    procedure Clear; override;

  end;

type
  { TJSONPair }
  TJSONPair = class(TJSONCore)
  // Private Declarations.
  private
    FName: TJSONValue;
    FValue: TJSONValue;
    procedure SetName(const AValue: TJSONValue);
    procedure SetValue(const AValue: TJSONValue);

  // Public Declarations.
  public
    constructor Create; overload; virtual;
    constructor Create(const AName: string; AValue: TJSONValue); overload;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    function Compose(AIndent: Integer = -1): string; override;
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; override;

    // These properties references the Name-Value Pair.
    property Name: TJSONValue read FName write SetName;
    property Value: TJSONValue read FValue write SetValue;

  end;

type
  { TJSONList }
  /// <summary>
  ///   The JSONValue Class forms the basis for all the JSON List Type
  ///   Value Types.
  /// </summary>
  TJSONList = class(TJSONValue)
  // Private Declarations.
  private
    FCount, FCapacity: Integer;
    FList: TJSONCoreArray;

  // Protected Declarations.
  protected
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    procedure SetCapacity(const ANewCapacity: Integer); override;

    function VariantToJSONValue(const AValue: Variant): TJSONValue;
    function VarRecToJSONValue(const AValue: TVarRec): TJSONValue;
    function RTTIValueToJSONValue(const AValue: TValue): TJSONValue;
    function ComposeValueAt(AIndex: Integer; AList: TJSONList;
      AIndent: Integer = -1): string; virtual;
    function ComposeList(ATokens: string; AIndent: Integer = -1): string; dynamic;
    function ApplyIndentation(AIndent: Integer = -1): Boolean; virtual;
    class procedure DecomposeValueAt(const ADataStr: string;
      var AIndex: Integer; AList: TJSONList); virtual;
    class function DecomposeList(ATokens: string; const ADataStr: string;
      var AIndex: Integer): TJSONValue; dynamic;

    function AddItem(AItem: TJSONCore): Integer; virtual;
    procedure InsertItem(AIndex: Integer; AItem: TJSONCore); virtual;
    procedure DeleteItem(AIndex: Integer); virtual;
    procedure Grow;

  // Public Declarations.
  public
    constructor Create; override;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    procedure Delete(AIndex: Integer);
    procedure Clear; override;

  end;

type
  { TJSONArray }
  /// <summary>
  ///   The JSONNull Class implements the functionality for a JSON Array Value.
  /// </summary>
  TJSONArray = class(TJSONList)
  // Protected Declarations.
  protected
    function GetItem(AIndex: Integer): TJSONValue; override;
    procedure SetItem(AIndex: Integer; const AValue: TJSONValue); override;

    function ComposeValueAt(AIndex: Integer; AList: TJSONList;
      AIndent: Integer = -1): string; override;
    function ApplyIndentation(AIndent: Integer = -1): Boolean; override;
    class procedure DecomposeValueAt(const ADataStr: string;
      var AIndex: Integer; AList: TJSONList); override;
    procedure ExtractValues(const AValue: array of const);

  // Public Declarations.
  public
    constructor Create; override;
    constructor Create(const AValue: array of const); overload;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    function Compose(AIndent: Integer = -1): string; override;
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; overload; override;
    class function ValueType: TJSONType; override;

    procedure AddNull;
    procedure AddBoolean(const AValue: Boolean);
    procedure AddInteger(const AValue: Int64);
    procedure AddFloat(const AValue: Extended);
    procedure AddDateTime(const AValue: TDateTime);
    procedure AddString(const AValue: string);
    procedure AddArray(const AValue: array of const);
    procedure AddObject(const AValue: TObject);
    procedure AddValue(const AValue: TJSONValue);
    procedure InsertNull(AIndex: Integer);
    procedure InsertBoolean(AIndex: Integer; const AValue: Boolean);
    procedure InsertInteger(AIndex: Integer; const AValue: Int64);
    procedure InsertFloat(AIndex: Integer; const AValue: Extended);
    procedure InsertDateTime(AIndex: Integer; const AValue: TDateTime);
    procedure InsertString(AIndex: Integer; const AValue: string);
    procedure InsertArray(AIndex: Integer; const AValue: array of const);
    procedure InsertObject(AIndex: Integer; const AValue: TObject);
    procedure InsertValue(AIndex: Integer; const AValue: TJSONValue);

  end;

type
  { TJSONObject }
  /// <summary>
  ///   The JSONNull Class implements the functionality for a JSON Object Value.
  /// </summary>
  TJSONObject = class(TJSONList)
  // Private Declarations.
  private
    FValidIndex: Boolean;
    FHashBuckets: TArray<TStringList>;

  // Protected Declarations.
  protected
    function GetItem(AIndex: Integer): TJSONValue; override;
    function GetMember(AName: string): TJSONValue; override;
    function GetMemberName(AIndex: Integer): string; override;
    procedure SetItem(AIndex: Integer; const AValue: TJSONValue); override;
    procedure SetMember(AName: string; const AValue: TJSONValue); override;

    function ComposeValueAt(AIndex: Integer; AList: TJSONList;
      AIndent: Integer = -1): string; override;
    class procedure DecomposeValueAt(const ADataStr: string;
      var AIndex: Integer; AList: TJSONList); override;
    procedure ExtractValues(const AObjValue: TObject);

    function AddItem(AItem: TJSONCore): Integer; override;
    procedure InsertItem(AIndex: Integer; AItem: TJSONCore); override;
    procedure DeleteItem(AIndex: Integer); override;

    function CalcHashBucketIndex(const AName: string): Cardinal;
    function FindIndex(const AName: string; var AIndex: Integer): Boolean;
    procedure AddIndex(const AName: string; AIndex: Integer);
    procedure InitializeIndex;
    procedure FinalizeIndex;
    procedure ClearIndex;
    procedure Reindex;

  // Public Declarations.
  public
    constructor Create; override;
    constructor Create(const AObjValue: TObject); overload;
    destructor Destroy; override;

    function EstimatedSize(AIndent: Integer = -1): Integer; override;
    function Compose(AIndent: Integer = -1): string; override;
    class function Decompose(const ADataStr: string;
      var AIndex: Integer): TJSONValue; overload; override;
    class function ValueType: TJSONType; override;
    procedure ImportFromObject(AObject: TObject);
    procedure ExportToObject(AObject: TObject);

    function FindMemberValue(AName: string; var AValue: TJSONValue): Boolean; override;

    procedure AddNull(AName: string);
    procedure AddBoolean(AName: string; const AValue: Boolean);
    procedure AddInteger(AName: string; const AValue: Int64);
    procedure AddFloat(AName: string; const AValue: Extended);
    procedure AddDateTime(AName: string; const AValue: TDateTime);
    procedure AddString(AName: string; const AValue: string);
    procedure AddArray(AName: string; const AValue: array of const);
    procedure AddObject(AName: string; const AValue: TObject);
    procedure AddValue(AName: string; const AValue: TJSONValue);
    procedure InsertNull(AIndex: Integer; AName: string);
    procedure InsertBoolean(AIndex: Integer; AName: string; const AValue: Boolean);
    procedure InsertInteger(AIndex: Integer; AName: string; const AValue: Int64);
    procedure InsertFloat(AIndex: Integer; AName: string; const AValue: Extended);
    procedure InsertDateTime(AIndex: Integer; AName: string; const AValue: TDateTime);
    procedure InsertString(AIndex: Integer; AName: string; const AValue: string);
    procedure InsertArray(AIndex: Integer; AName: string; const AValue: array of const);
    procedure InsertObject(AIndex: Integer; AName: string; const AValue: TObject);
    procedure InsertValue(AIndex: Integer; AName: string; const AValue: TJSONValue);
    procedure Clear; override;

  end;


const
{$IFNDEF DEBUG}
  CMaxHashBuckets = 4;
{$ELSE}
  CMaxHashBuckets = 16;
{$ENDIF}

resourcestring
  SJSONErrParser =
    'Error Parsing JSON %s Value.' + SLineBreak +
    '- Error: %s' + SLineBreak +
    '- DataStr: %s';
  SJSONErrInvalidType =
    'Invalid Typecast: %s to %s';
  SJSONErrOutOfBounds =
    'JSONList %s out of bounds (%d)';

implementation

{==============================================================================}
{ Helper Routines }

procedure DisposeAndNil(var AInstance);
{$IFNDEF DEBUG}inline;{$ENDIF}
// This routine will DisposeOf an Object and set the reference to nil.
var
  ObjInstance: TObject absolute AInstance;
begin
  ObjInstance.DisposeOf;
  ObjInstance := nil;
end;

procedure SkipWhiteSpaces(ADataStr: string; var AIndex: Integer);
{$IFNDEF DEBUG}inline;{$ENDIF}
// This routine will Skip any Leagal White Spaces on a JSON string.
begin
  if ADataStr > EmptyStr then
    while (ADataStr.Chars[AIndex] > #00) and (ADataStr.Chars[AIndex] <= #32) do
      Inc(AIndex);
end;

function ExtractNumberType(ADataStr: string): TJSONNumberType;
{$IFNDEF DEBUG}inline;{$ENDIF}
// This routine will Return the type of the Numeric Value stored in ADataStr.
var
  Index: Integer;
begin
  Index := 0;
  Result := ExtractNumberType(ADataStr, Index);
end;

function ExtractNumberType(ADataStr: string; var AIndex: Integer): TJSONNumberType;
{$IFNDEF DEBUG}inline;{$ENDIF}
// This routine will Return the type of the Numeric Value stored in ADataStr.
// Please NOTE: At the end of this routine, AIndex will point to the first
//    character after the number.
const
  CIntegerChars = ['0'..'9', '-', '+'];
  CFloatChars = ['.', 'e', 'E'];
  CDateChars = [':', '/', '\', ' ', 'a'..'z', 'A'..'Z'];
var
  TerminateLoop: Boolean;
begin
  SkipWhiteSpaces(ADataStr, AIndex);
  if not CharInSet(ADataStr.Chars[AIndex], CIntegerChars) then
    Result := ntNone
  else
  begin
    Result := ntInteger;
    repeat
      if Result = ntInteger then
      begin
        if CharInSet(ADataStr.Chars[AIndex], CFloatChars) then Result := ntFloat
        else if CharInSet(ADataStr.Chars[AIndex], CDateChars) then Result := ntDateTime;
      end;

      case Result of
        ntInteger: TerminateLoop := not CharInSet(ADataStr.Chars[AIndex], CIntegerChars);
        ntFloat: TerminateLoop := not CharInSet(ADataStr.Chars[AIndex], CIntegerChars + CFloatChars);
        ntDateTime: TerminateLoop := not CharInSet(ADataStr.Chars[AIndex], CIntegerChars + CDateChars);
      else
        TerminateLoop := True;
      end;

      if not TerminateLoop then Inc(AIndex);
    until TerminateLoop;
  end;
end;

function JSONTypeToStr(AType: TJSONType): string;
{$IFNDEF DEBUG}inline;{$ENDIF}
// This routine will Return a Human Friendly Type String.
begin
  case AType of
    jtNull: Result := 'Null';
    jtBoolean: Result := 'Boolean';
    jtNumber: Result := 'Number';
    jtString: Result := 'String';
    jtArray: Result := 'Array';
    jtObject: Result := 'Object';
  else
    Result := 'Unknown';
  end;
end;

procedure RaiseParserError(const ADataStr: string; AIndex: Integer;
  ADataDescr, AParseError: string);
{$IFNDEF DEBUG}inline;{$ENDIF}
// This routine will Raised a Parse Error Message.
begin
  raise EJSONParseError.CreateFmt(SJSONErrParser, [ADataDescr,
    AParseError, ADataStr.Substring(AIndex, AIndex + 20)]);
end;

procedure RaiseParserErrorFmt(const ADataStr: string; AIndex: Integer;
  ADataDescr, AParseError: string; Args: array of const);
// This routine will Raised a Parse Error Message.
begin
  raise EJSONParseError.CreateFmt(SJSONErrParser, [ADataDescr,
    Format(AParseError, Args), ADataStr.Substring(AIndex, AIndex + 20)]);
end;

procedure ValidateToken(AExpectedToken: Char; ADataStr: string;
  var AIndex: Integer; ADataDescr: string = 'Data');
{$IFNDEF DEBUG}inline;{$ENDIF}
// This routine will Validate the Token at the giben Index.
begin
  if ADataStr.Chars[AIndex] <> AExpectedToken then
    RaiseParserErrorFmt(ADataStr, AIndex, ADataDescr,
      'Invalid Token: <%s> expected, but <%s> found.',
      [AExpectedToken, ADataStr.Chars[AIndex]]);
  Inc(AIndex); SkipWhiteSpaces(ADataStr, AIndex);
end;

{==============================================================================}
{ TJSONValue }

procedure TJSONValue.AfterConstruction;
// This method will execute after the last constructor.
begin
  inherited AfterConstruction;
end;

procedure TJSONValue.BeforeDestruction;
// This method will execute before the first destructor.
begin
  Clear;
  inherited BeforeDestruction;
end;

procedure TJSONValue.Clear;
// This method will Clear the Internal Data Structure.
begin
end;

function TJSONValue.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
begin
  Result := EmptyStr;
end;

constructor TJSONValue.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
end;

class function TJSONValue.Decompose(const ADataStr: string): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
var
  Index: Integer;
begin
  Index := 0;
  Result := Decompose(ADataStr, Index);
end;

class function TJSONValue.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
begin
  Assert(Self = TJSONValue, Format('%s.Decompose not implemented.', [Self.ClassName]));
  if ADataStr = EmptyStr then RaiseParserError(ADataStr,
    AIndex, JSONTypeToStr(ValueType), 'Invalid JSON Data Text.');

  SkipWhiteSpaces(ADataStr, AIndex);
  case ADataStr.Chars[AIndex] of
    'n': Result := TJSONNull.Decompose(ADataStr, AIndex);
    'f', 't': Result := TJSONBoolean.Decompose(ADataStr, AIndex);
    '0'..'9', '-': Result := TJSONNumber.Decompose(ADataStr, AIndex);
    '"': Result := TJSONString.Decompose(ADataStr, AIndex);
    '{': Result := TJSONObject.Decompose(ADataStr, AIndex);
    '[': Result := TJSONArray.Decompose(ADataStr, AIndex);
  else
    Result := nil;
    RaiseParserError(ADataStr, AIndex, JSONTypeToStr(ValueType),
      'Unknown token value.');
  end;
end;

destructor TJSONValue.Destroy;
// This method destructs an object instance.
begin
  inherited Destroy;
end;

function TJSONValue.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
  Result := 0;
end;

function TJSONValue.FindMemberValue(AName: string;
  var AValue: TJSONValue): Boolean;
// This method will Find the Member with the given Name, if it exists and
// return the Value for it.
begin
  Result := False;
  AValue := nil;
end;

function TJSONValue.GetCapacity: Integer;
// Read spesifier method for the Capacity property.
begin
  Result := 0;
end;

function TJSONValue.GetCount: Integer;
// Read spesifier method for the Count property.
begin
  Result := 0;
end;

function TJSONValue.GetItem(AIndex: Integer): TJSONValue;
// Read spesifier method for the Item property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['List', ClassName]);
end;

function TJSONValue.GetMember(AName: string): TJSONValue;
// Read spesifier method for the Member property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Object', ClassName]);
end;

function TJSONValue.GetMemberName(AIndex: Integer): string;
// Read spesifier method for the MemberName property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Object', ClassName]);
end;

function TJSONValue.GetValue: Variant;
// Read spesifier method for the Value property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Variant', ClassName]);
end;

function TJSONValue.GetValueAsBoolean: Boolean;
// Read spesifier method for the ValueAsBoolean property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Boolean', ClassName]);
end;

function TJSONValue.GetValueAsDateTime: TDateTime;
// Read spesifier method for the ValueAsDateTime property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['DateTime', ClassName]);
end;

function TJSONValue.GetValueAsFloat: Extended;
// Read spesifier method for the ValueAsFloat property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Float', ClassName]);
end;

function TJSONValue.GetValueAsInteger: Int64;
// Read spesifier method for the ValueAsInteger property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Integer', ClassName]);
end;

function TJSONValue.GetValueAsString: string;
// Read spesifier method for the ValueAsString property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['String', ClassName]);
end;

procedure TJSONValue.SetCapacity(const ANewCapacity: Integer);
// Write spesifier method for the Capacity property.
begin
end;

procedure TJSONValue.SetItem(AIndex: Integer; const AValue: TJSONValue);
// Write spesifier method for the Item property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['List', ClassName]);
end;

procedure TJSONValue.SetMember(AName: string; const AValue: TJSONValue);
// Write spesifier method for the Member property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Object', ClassName]);
end;

procedure TJSONValue.SetValue(const AValue: Variant);
// Write spesifier method for the Variant property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Variant', ClassName]);
end;

procedure TJSONValue.SetValueAsBoolean(const AValue: Boolean);
// Write spesifier method for the ValueAsBoolean property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Boolean', ClassName]);
end;

procedure TJSONValue.SetValueAsDateTime(const AValue: TDateTime);
// Write spesifier method for the ValueAsDateTime property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['DateTime', ClassName]);
end;

procedure TJSONValue.SetValueAsFloat(const AValue: Extended);
// Write spesifier method for the ValueAsFloat property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Float', ClassName]);
end;

procedure TJSONValue.SetValueAsInteger(const AValue: Int64);
// Write spesifier method for the ValueAsInteger property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['Integer', ClassName]);
end;

procedure TJSONValue.SetValueAsString(const AValue: string);
// Write spesifier method for the ValueAsString property.
begin
  raise EJSONConvertError.CreateFmt(SJSONErrInvalidType, ['String', ClassName]);
end;

class function TJSONValue.ValueType: TJSONType;
// This method will Return the JSON Type of the Value stored.
begin
  Result := jtUnknown;
end;

{==============================================================================}
{ TJSONNull }

function TJSONNull.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
begin
  Result := 'null';
end;

constructor TJSONNull.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
end;

class function TJSONNull.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
begin
  SkipWhiteSpaces(ADataStr, AIndex);
  if not ADataStr.Substring(AIndex, 4).Equals('null') then
    RaiseParserErrorFmt(ADataStr, AIndex, 'Null',
      'Invalid Token: <%s> expected, but <%s> found.',
      ['null', ADataStr.Substring(AIndex, 4)]);
  Inc(AIndex, 4);
  Result := TJSONNull.Create;
end;

destructor TJSONNull.Destroy;
// This method destructs an object instance.
begin
  inherited Destroy;
end;

function TJSONNull.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
  Result := 4;
end;

function TJSONNull.GetValue: Variant;
// Read spesifier method for the Value property.
begin
  Result := varNull;
end;

class function TJSONNull.ValueType: TJSONType;
// This method will Return the JSON Type of the Value stored.
begin
  Result := jtNull;
end;

{==============================================================================}
{ TJSONBoolean }

procedure TJSONBoolean.Clear;
// This method will Clear the Internal Data Structure.
begin
  FValue := False;
end;

function TJSONBoolean.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
begin
  if FValue then
    Result := 'true' else
    result := 'false';
end;

constructor TJSONBoolean.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
end;

constructor TJSONBoolean.Create(const AValue: Boolean);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  SetValueAsBoolean(AValue);
end;

class function TJSONBoolean.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
begin
  if ADataStr = EmptyStr then RaiseParserError(ADataStr,
    AIndex, 'Boolean', 'Invalid JSON Data Text.');

  SkipWhiteSpaces(ADataStr, AIndex);
  if ADataStr.Substring(AIndex, 4).Equals('true') then
  begin
    Result := TJSONBoolean.Create(True);
    Inc(AIndex, 4);
  end
  else if ADataStr.Substring(AIndex, 5).Equals('false') then
  begin
    Result := TJSONBoolean.Create(False);
    Inc(AIndex, 5);
  end
  else
  begin
    Result := nil;
    RaiseParserError(ADataStr, AIndex, 'Boolean',
      'Boolean tokens <true> or <false> expected.');
  end;
end;

destructor TJSONBoolean.Destroy;
// This method destructs an object instance.
begin
  inherited Destroy;
end;

function TJSONBoolean.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
    if FValue then
    Result := 4 else
    Result := 5;
end;

function TJSONBoolean.GetValue: Variant;
// Read spesifief method for the Value property.
begin
  Result := FValue;
end;

function TJSONBoolean.GetValueAsBoolean: Boolean;
// Read spesifief method for the ValueAsBoolean property.
begin
  Result := FValue;
end;

function TJSONBoolean.GetValueAsFloat: Extended;
// Read spesifief method for the ValueAsFloat property.
begin
  if FValue then
    Result := 1.00 else
    Result := 0.00;
end;

function TJSONBoolean.GetValueAsInteger: Int64;
// Read spesifief method for the ValueAsInteger property.
begin
  if FValue then
    Result := -1 else
    Result := 0;
end;

function TJSONBoolean.GetValueAsString: string;
// Read spesifief method for the ValueAsString property.
begin
  Result := BoolToStr(FValue, True);;
end;

procedure TJSONBoolean.SetValue(const AValue: Variant);
// Write spesifier method for the Value property.
begin
  try
    FValue := AValue;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONBoolean.SetValueAsBoolean(const AValue: Boolean);
// Write spesifier method for the ValueAsBoolean property.
begin
  try
    FValue := AValue;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONBoolean.SetValueAsFloat(const AValue: Extended);
// Write spesifier method for the ValueAsFloat property.
begin
  try
    FValue := (AValue <= -1) or (AValue >= 1);
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONBoolean.SetValueAsInteger(const AValue: Int64);
// Write spesifier method for the ValueAsInteger property.
begin
  try
    FValue := Boolean(AValue);
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONBoolean.SetValueAsString(const AValue: string);
// Write spesifier method for the ValueAsString property.
begin
  try
    FValue := StrToBool(AValue);
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

class function TJSONBoolean.ValueType: TJSONType;
// This method will Return the JSON Type of the Value stored.
begin
  Result := jtBoolean;
end;

{==============================================================================}
{ TJSONNumber }

procedure TJSONNumber.Clear;
// This method will Clear the Internal Data Structure.
begin
  FIntValue := 0;
  FFloatValue := 0.00;
  FNumberType := ntInteger;
end;

function TJSONNumber.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
begin
  if FNumberType = ntInteger then
    Result := IntToStr(FIntValue) else
    Result := FloatToStr(FFloatValue);
end;

constructor TJSONNumber.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
end;

constructor TJSONNumber.Create(const AValue: Int64);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  SetValueAsInteger(AValue);
end;

constructor TJSONNumber.Create(const AValue: Extended);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  SetValueAsFloat(AValue);
end;

constructor TJSONNumber.Create(const AValue: TDateTime);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  SetValueAsDateTime(AValue);
end;

class function TJSONNumber.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
var
  Index: Integer;
  NumberType: TJSONNumberType;
begin
  if ADataStr = EmptyStr then RaiseParserError(ADataStr,
    AIndex, 'Number', 'Invalid JSON Data Text.');

  Index := AIndex;
  NumberType := ExtractNumberType(ADataStr, AIndex);
  try
    case NumberType of
      ntInteger: Result := TJSONNumber.Create(StrToInt64(
        ADataStr.Substring(Index, AIndex-Index)));
      ntFloat: Result := TJSONNumber.Create(StrToFloat(
        ADataStr.Substring(Index, AIndex-Index)));
      ntDateTime: Result := TJSONNumber.Create(StrToDateTime(
        ADataStr.Substring(Index, AIndex-Index)));
    else
      Result := nil;
      RaiseParserError(ADataStr, Index, 'Number', Format(
        'Invalid Token: Number expected, but <%s> found.',
        [ADataStr.Substring(AIndex, 5)]));
    end;
  except
    on E: Exception do
    begin
      Result := nil;
      RaiseParserError(ADataStr, Index, 'Number', E.Message);
    end;
  end;
end;

destructor TJSONNumber.Destroy;
// This method destructs an object instance.
begin
  inherited Destroy;
end;

function TJSONNumber.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
  Result := GetValueAsString.Length;
end;

function TJSONNumber.GetValue: Variant;
// Read spesifier method for the Value property.
var
  DateTime: TDateTime;
begin
  case FNumberType of
    ntInteger: Result := FIntValue;
    ntFloat: Result := FFloatValue;
    ntDateTime:
    begin
      DateTime := FFloatValue;
      Result := DateTime;
    end;
  end;
end;

function TJSONNumber.GetValueAsBoolean: Boolean;
// Read spesifier method for the ValueAsBoolean property.
begin
  if FNumberType = ntInteger then
    Result := FIntValue <> 0 else
    Result := (FFloatValue <= -1.0) or (FFloatValue >= 1.0);
end;

function TJSONNumber.GetValueAsDateTime: TDateTime;
// Read spesifier method for the ValueAsDateTime property.
begin
  if FNumberType = ntInteger then
    Result := FIntValue else
    Result := FFloatValue;
end;

function TJSONNumber.GetValueAsFloat: Extended;
// Read spesifier method for the ValueAsFloat property.
begin
  if FNumberType = ntInteger then
    Result := FIntValue else
    Result := FFloatValue;
end;

function TJSONNumber.GetValueAsInteger: Int64;
// Read spesifier method for the ValueAsInteger property.
begin
  if FNumberType = ntInteger then
    Result := FIntValue else
    Result := Round(FFloatValue);
end;

function TJSONNumber.GetValueAsString: string;
// Read spesifier method for the ValueAsString property.
begin
  case FNumberType of
    ntInteger: Result := IntToStr(FIntValue);
    ntFloat: Result := FloatToStr(FFloatValue);
    ntDateTime: Result := DateTimeToStr(FFloatValue);
  end;
end;

procedure TJSONNumber.SetValue(const AValue: Variant);
// Write spesifier method for the Value property.
begin
  try
    // Set a string to match the type
    case VarType(AValue) and VarTypeMask of
      varBoolean:
        SetValueAsBoolean(AValue);
      varSmallInt, varInteger, varInt64, varByte, varWord, varLongWord:
        SetValueAsInteger(AValue);
      varSingle, varDouble, varCurrency, varDate:
        SetValueAsFloat(AValue);
      varOleStr, varString:
        SetValueAsString(AValue);
    else
      inherited SetValue(AValue);
    end;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONNumber.SetValueAsBoolean(const AValue: Boolean);
// Write spesifier method for the ValueAsBoolean property.
begin
  try
    FNumberType := ntInteger;
    if AValue then
      FIntValue := -1 else
      FIntValue := 0;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONNumber.SetValueAsDateTime(const AValue: TDateTime);
// Write spesifier method for the ValueAsDateTime property.
begin
  try
    FNumberType := ntDateTime;
    FFloatValue := AValue;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONNumber.SetValueAsFloat(const AValue: Extended);
// Write spesifier method for the ValueAsFloat property.
begin
  try
    FNumberType := ntFloat;
    FFloatValue := AValue;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONNumber.SetValueAsInteger(const AValue: Int64);
// Write spesifier method for the ValueAsInteger property.
begin
  try
    FNumberType := ntInteger;
    FIntValue := AValue;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONNumber.SetValueAsString(const AValue: string);
// Write spesifier method for the ValueAsString property.
begin
  try
    if AValue = '' then
      inherited SetValueAsString(AValue)
    else
    begin
      FNumberType := ExtractNumberType(AValue);
      case FNumberType of
        ntInteger: FIntValue := StrToInt64(AValue);
        ntFloat: FFloatValue := StrToFloat(AValue);
        ntDateTime: FFloatValue := StrToDateTime(AValue);
      else
        raise EJSONConvertError.Create('Invalid numeric string value.');
      end;
    end;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

class function TJSONNumber.ValueType: TJSONType;
// This method will Return the JSON Type of the Value stored.
begin
  Result := jtNumber;
end;

{==============================================================================}
{ TJSONString }

procedure TJSONString.Clear;
// This method will Clear the Internal Data Structure.
begin
  FValue := EmptyStr;
end;

function TJSONString.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
var
  Index: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create(EstimatedSize);
  try
    Builder.Append('"');
    try
      for Index := 0 to FValue.Length-1 do
      begin
        case FValue.Chars[Index] of
          '"': Builder.Append('\"');
          '\': Builder.Append('\\');
          '/': Builder.Append('\/');
          #08: Builder.Append('\b');
          #09: Builder.Append('\t');
          #10: Builder.Append('\n');
          #12: Builder.Append('\f');
          #13: Builder.Append('\r');
        else
          if (FValue.Chars[Index] < #32) or (FValue.Chars[Index] > #127) then
            Builder.AppendFormat('\u%.4x', [Ord(FValue.Chars[Index])]) else
            Builder.Append(FValue.Chars[Index]);
        end;
      end;
    finally
      Result := Builder.Append('"').ToString;
    end;
  finally
    Builder.DisposeOf;
  end;
end;

constructor TJSONString.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
end;

constructor TJSONString.Create(const AValue: string);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  SetValueAsString(AValue);
end;

constructor TJSONString.Create(const AValue: WideString);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  SetValueAsString(string(AValue));
end;

constructor TJSONString.Create(const AValue: AnsiString);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  SetValueAsString(string(AValue));
end;

class function TJSONString.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
var
  Index: Integer;
  ValueLength: Integer;
  Builder: TStringBuilder;
begin
  if ADataStr = EmptyStr then RaiseParserError(ADataStr,
    AIndex, 'String', 'Invalid JSON Data Text.');

  Index := AIndex;
  SkipWhiteSpaces(ADataStr, Index);
  Builder := TStringBuilder.Create(2*Length(ADataStr));
  try
    // Validate Opening of String ...
    ValidateToken('"', ADataStr, Index, 'String');
    try
      // Extract Data Value ...
      ValueLength := Length(ADataStr);
      while (ADataStr.Chars[Index] <> '"') and (Index < ValueLength) do
      begin
        if ADataStr.Chars[Index] <> '\' then
          Builder.Append(ADataStr.Chars[Index])
        else
        begin
          Inc(Index);
          case ADataStr.Chars[Index] of
            '"': Builder.Append('"');
            '\': Builder.Append('\');
            '/': Builder.Append('/');
            'b': Builder.Append(#08);
            't': Builder.Append(#09);
            'n': Builder.Append(#10);
            'f': Builder.Append(#12);
            'r': Builder.Append(#13);
            'u':
            begin
              Builder.Append(Chr(StrToInt('$'+ADataStr.Substring(Index+1, 4))));
              Inc(Index, 4);
            end;
          end;
        end;
        Inc(Index);
      end;
    finally
      // Validate Closing of String ...
      ValidateToken('"', ADataStr, Index, 'String');
      AIndex := Index;
    end;
  finally
    Result := TJSONString.Create(Builder.ToString);
    Builder.DisposeOf;
  end;
end;

destructor TJSONString.Destroy;
// This method destructs an object instance.
begin
  inherited Destroy;
end;

function TJSONString.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
  Result := 6*FValue.Length;
end;

function TJSONString.GetValue: Variant;
// Read spesifier method for the Value property.
begin
  Result := FValue;
end;

function TJSONString.GetValueAsBoolean: Boolean;
// Read spesifier method for the ValueAsBoolean property.
begin
  Result := StrToBool(FValue);
end;

function TJSONString.GetValueAsDateTime: TDateTime;
// Read spesifier method for the ValueAsDateTime property.
begin
  Result := StrToDateTime(FValue);
end;

function TJSONString.GetValueAsFloat: Extended;
// Read spesifier method for the ValueAsFloat property.
begin
  Result := StrToFloat(FValue);
end;

function TJSONString.GetValueAsInteger: Int64;
// Read spesifier method for the ValueAsInteger property.
begin
  Result := Round(StrToFloat(FValue));
end;

function TJSONString.GetValueAsString: string;
// Read spesifier method for the ValueAsString property.
begin
  Result := FValue;
end;

procedure TJSONString.SetValue(const AValue: Variant);
// Write spesifier method for the Value property.
begin
  try
    FValue := AValue;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONString.SetValueAsBoolean(const AValue: Boolean);
// Write spesifier method for the ValueAsBoolean property.
begin
  try
    FValue := BoolToStr(AValue, True);
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONString.SetValueAsDateTime(const AValue: TDateTime);
// Write spesifier method for the ValueAsDateTime property.
begin
  try
    FValue := DateTimeToStr(AValue);
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONString.SetValueAsFloat(const AValue: Extended);
// Write spesifier method for the ValueAsFloat property.
begin
  try
    FValue := FloatToStr(AValue);
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONString.SetValueAsInteger(const AValue: Int64);
// Write spesifier method for the ValueAsInteger property.
begin
  try
    FValue := IntToStr(AValue);
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

procedure TJSONString.SetValueAsString(const AValue: string);
// Write spesifier method for the ValueAsString property.
begin
  try
    FValue := AValue;
  except on E: Exception do
    raise EJSONConvertError.Create(E.Message);
  end;
end;

class function TJSONString.ValueType: TJSONType;
// This method will Return the JSON Type of the Value stored.
begin
  Result := jtString;
end;

{==============================================================================}
{ TJSONList }

function TJSONList.AddItem(AItem: TJSONCore): Integer;
// This method will Add a JSONValue Item to the Internal Data Structure.
begin
  if not Assigned(AItem) then raise EJSONError.CreateFmt(
    'Cannot add an unassigned JSONValue to the %s list object.',
    [ClassName]);

  Result := FCount;
  if Result = FCapacity then Grow;
  FList[Result] := AItem;
  Inc(FCount);
end;

function TJSONList.ApplyIndentation(AIndent: Integer): Boolean;
// This method will Return True if intentation should be applied or not..
begin
  Result := AIndent >= 0;
end;

procedure TJSONList.Clear;
// This method will Clear the Internal Data Structure.
var
  Index: Integer;
begin
  for Index := FCount-1 downto 0 do DeleteItem(Index);
  SetCapacity(0);
end;

function TJSONList.ComposeList(ATokens: string; AIndent: Integer): string;
// This routine will Compose the JSON string.
var
  Index: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create(EstimatedSize);
  try
    Builder.Append(ATokens.Chars[0]);
    try
      if ApplyIndentation(AIndent) then Inc(AIndent, CIndentSize);
      if FCount > 0 then
        Builder.Append(ComposeValueAt(0, Self, AIndent));
      for Index := 1 to FCount-1 do
      begin
        Builder.Append(',');
        Builder.Append(ComposeValueAt(Index, Self, AIndent));
      end;
    finally
      if ApplyIndentation(AIndent) then
      begin
        Dec(AIndent, CIndentSize);
        if Count > 0 then Builder.AppendLine.Append(DupeString(' ', AIndent));
      end;
      Result := Builder.Append(ATokens.Chars[1]).ToString;
    end;
  finally
    Builder.DisposeOf;
  end;
end;

function TJSONList.ComposeValueAt(AIndex: Integer; AList: TJSONList;
  AIndent: Integer): string;
// This routine will Compose the JSON string for the given Item.
begin
  if ApplyIndentation(AIndent) then
    Result := sLineBreak + DupeString(' ', AIndent) else
    Result := '';
end;

constructor TJSONList.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
end;

class function TJSONList.DecomposeList(ATokens: string; const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
var
  Index: Integer;
begin
  Result := Self.Create;
  try
    Index := AIndex;
    SkipWhiteSpaces(ADataStr, Index);
    // Validate Opening of Array ...
    ValidateToken(ATokens.Chars[0], ADataStr, Index, 'List');
    try
      // Extract the First Data Value ...
      if ADataStr.Chars[Index] <> ATokens.Chars[1] then
        DecomposeValueAt(ADataStr, Index, TJSONList(Result));
      // Extract the Remaining Data Value(s) ...
      while ADataStr.Chars[Index] <> ATokens.Chars[1] do
      begin
        // Validate Array Item Seperator ...
        ValidateToken(',', ADataStr, Index, 'List');
        // Extract the Data Value ...
        DecomposeValueAt(ADataStr, Index, TJSONList(Result));
      end;
    finally
      // Validate Closing of Array ...
      ValidateToken(ATokens.Chars[1], ADataStr, Index, 'List');
      AIndex := Index;
    end;
  except
    Result.DisposeOf;
    raise;
  end;
end;

class procedure TJSONList.DecomposeValueAt(const ADataStr: string;
  var AIndex: Integer; AList: TJSONList);
// This method will Decompose the Value at the given Index.
begin
  SkipWhiteSpaces(ADataStr, AIndex);
end;

procedure TJSONList.Delete(AIndex: Integer);
// This method will Delete a JSONValue Item from the Internal Data Structure.
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['index', AIndex]);
  DeleteItem(AIndex);
end;

procedure TJSONList.DeleteItem(AIndex: Integer);
// This method will Delete a JSONValue Item from the Internal Data Structure.
begin
  Dec(FCount);
  FList[AIndex].DisposeOf;
  if AIndex < FCount then System.Move(FList[AIndex + 1], FList[AIndex],
    (FCount - AIndex) * SizeOf(Pointer));
end;

destructor TJSONList.Destroy;
// This method destructs an object instance.
begin
  inherited Destroy;
end;

function TJSONList.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
var
  Index: Integer;
begin
  if AIndent < 0 then Result := 2 else Result := 6;
  for Index := 0 to FCount-1 do
  begin
    Inc(Result, 2);
    Inc(Result, FList[Index].EstimatedSize);
    if AIndent > 0 then Inc(Result, AIndent+2);
  end;
end;

function TJSONList.GetCapacity: Integer;
// Read spesifier method for the Capacity property.
begin
  Result := FCapacity;
end;

function TJSONList.GetCount: Integer;
// Read spesifier method for the Count property.
begin
  Result := FCount;
end;

procedure TJSONList.Grow;
// This method will Grow the Internal Data Structure.
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TJSONList.InsertItem(AIndex: Integer; AItem: TJSONCore);
// This method will Insert a JSONValue Item to the Internal Data Structure
// at the given AIndex.
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['index', AIndex]);

  if FCount = FCapacity then Grow;
  if AIndex < FCount then System.Move(FList[AIndex], FList[AIndex + 1],
    (FCount - AIndex) * SizeOf(Pointer));
  FList[AIndex] := AItem;
  Inc(FCount);
end;

function TJSONList.RTTIValueToJSONValue(const AValue: TValue): TJSONValue;
// This method will Translate a RTTI Value Object to a JSON Value Object.
var
  Index: Integer;
begin
  case AValue.Kind of
    tkFloat: Result := TJSONNumber.Create(AValue.AsExtended);
    tkInteger, tkInt64: Result := TJSONNumber.Create(AValue.AsOrdinal);
    tkChar, tkWChar: Result := TJSONString.Create(AValue.AsString);
    tkString, tkLString, tkWString, tkUString:
      Result := TJSONString.Create(AValue.AsString);
    tkVariant: Result := VariantToJSONValue(AValue.AsVariant);
    tkClassRef: Result := TJSONString.Create(AValue.AsClass.ClassName);
    tkClass: Result := TJSONObject.Create(AValue.AsObject);
    tkInterface: Result := TJSONString.Create('IInterface');

    tkEnumeration, tkSet:
    begin
      if AValue.IsType<Boolean> then
        Result := TJSONBoolean.Create(AValue.AsOrdinal <> 0) else
        Result := TJSONString.Create(AValue.ToString);
    end;

//DelME:
//    tkRecord:
//    begin
//      Result := TJSONObject.Create;
//      try
//        // Add code here ...
//      except
//        Result.Free;
//        raise;
//      end;
//    end;

    tkArray, tkDynArray:
    begin
      Result := TJSONArray.Create;
      try
        for Index := 0 to AValue.GetArrayLength-1 do
          TJSONArray(Result).AddValue(RTTIValueToJSONValue(
            AValue.GetArrayElement(Index)));
      except
        Result.Free;
        raise;
      end;
    end;

//DelME
//    tkUnknown: ;
//    tkPointer: ;
//    tkProcedure: ;
//    tkMethod: ;
  else
    Result := nil;
  end;

//DelME
//            case PropertyType.TypeKind of
//              tkString, tkLString, tkWString, tkUString:
//                AddString(Name, MemberValue.AsString);
//              tkChar, tkWChar: AddString(Name, MemberValue.AsString);
//              tkInteger, tkInt64: AddInteger(Name, MemberValue.AsOrdinal);
//              tkFloat: AddFloat(Name, MemberValue.AsExtended);
//              tkClass: AddObject(Name, MemberValue.AsObject);
//              tkClassRef: AddString(Name, MemberValue.AsClass.ClassName);
//              tkSet:
//              begin
//                AddInteger(Name, 0);
//              end;
//              tkEnumeration:
//              begin
//                if GetValue(AObjValue).IsType<Boolean> then
//                  AddBoolean(Name, MemberValue.AsOrdinal <> 0) else
//                  AddInteger(Name, MemberValue.AsOrdinal);
//              end;
//              tkArray, tkDynArray:
//              begin
//                JSONArray := TJSONArray.Create;
//                try
//                  for Index := 0 to MemberValue.GetArrayLength do
//                  begin
//                    Value := MemberValue.GetArrayElement(Index);
//                    JSONArray.a
//                  end;
//                except
//                  JSONArray.Free;
//                  raise;
//                end;
//
//                AddArray(Name, []);
//              end;
//              tkVariant:
//              begin
//                case VarType(MemberValue.asVariant) and VarTypeMask of
//                  varBoolean:
//                    AddBoolean(Name, MemberValue.asVariant);
//                  varSmallInt, varInteger, varInt64, varByte, varWord, varLongWord:
//                    AddInteger(Name, MemberValue.asVariant);
//                  varSingle, varDouble, varCurrency, varDate:
//                    AddFloat(Name, MemberValue.asVariant);
//                  varOleStr, varString, varUString:
//                    AddString(Name, MemberValue.asVariant);
//                else
//                  raise EJSONError.CreateFmt(
//                    'Invalid typecast for Variant Type for %s',
//                    [Name]);
//                end;
//              end;
//            else
//              AddString(Name, GetEnumName(TypeInfo(TTypeKind),
//                Ord(PropertyType.TypeKind)));
////              raise EJSONError.CreateFmt(
////                'Invalid typecast for the %s member',
////                [Name]);
//            end;
end;

procedure TJSONList.SetCapacity(const ANewCapacity: Integer);
// Write spesifier method for the Capacity property.
begin
  if ANewCapacity < FCount then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['capacity', ANewCapacity]);

  if ANewCapacity <> FCapacity then
  begin
    SetLength(FList, ANewCapacity);
    FCapacity := ANewCapacity;
  end;
end;

function TJSONList.VariantToJSONValue(const AValue: Variant): TJSONValue;
// This method will Translate a Variant Structure to a JSON Value Object.
begin
  case VarType(AValue) and VarTypeMask of
    //varEmpty    = $0000; { vt_empty        0 }
    varEmpty: Result := TJSONNull.Create;
    //varNull     = $0001; { vt_null         1 }
    varNull: Result := TJSONNull.Create;
    //varSmallint = $0002; { vt_i2           2 }
    varSmallint: Result := TJSONNumber.Create(Int64(AValue));
    //varInteger  = $0003; { vt_i4           3 }
    varInteger: Result := TJSONNumber.Create(Int64(AValue));
    //varSingle   = $0004; { vt_r4           4 }
    varSingle: Result := TJSONNumber.Create(Extended(AValue));
    //varDouble   = $0005; { vt_r8           5 }
    varDouble: Result := TJSONNumber.Create(Extended(AValue));
    //varCurrency = $0006; { vt_cy           6 }
    varCurrency: Result := TJSONNumber.Create(Extended(AValue));
    //varDate     = $0007; { vt_date         7 }
    varDate: Result := TJSONNumber.Create(VarToDateTime(AValue));
    //varOleStr   = $0008; { vt_bstr         8 }
    varOleStr: Result := TJSONString.Create(VarToStr(AValue));
    //varDispatch = $0009; { vt_dispatch     9 }
    varDispatch: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varDispatch');
    //varError    = $000A; { vt_error       10 }
    varError: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varError');
    //varBoolean  = $000B; { vt_bool        11 }
    varBoolean: Result := TJSONBoolean.Create(AValue);
    //varVariant  = $000C; { vt_variant     12 }
    varVariant: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varVariant');
    //varUnknown  = $000D; { vt_unknown     13 }
    varUnknown: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varUnknown');
    //varShortInt = $0010; { vt_i1          16 }
    varShortInt: Result := TJSONNumber.Create(Int64(AValue));
    //varByte     = $0011; { vt_ui1         17 }
    varByte: Result := TJSONNumber.Create(Int64(AValue));
    //varWord     = $0012; { vt_ui2         18 }
    varWord: Result := TJSONNumber.Create(Int64(AValue));
    //varLongWord = $0013; { vt_ui4         19 }
    varLongWord: Result := TJSONNumber.Create(Int64(AValue));
    //varInt64    = $0014; { vt_i8          20 }
    varInt64: Result := TJSONNumber.Create(Int64(AValue));
    //varUInt64   = $0015; { vt_ui8         21 }
    varUInt64: Result := TJSONNumber.Create(Int64(AValue));
    //varRecord   = $0024; { VT_RECORD      36 }
    varRecord: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varRecord');
    //varStrArg   = $0048; { vt_clsid        72 }
    varStrArg: Result := TJSONString.Create(VarToStr(AValue));
    //varObject   = $0049; {                 73 }
    //varObject: Result := TJSONObject.Create(AValue);
    varObject: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varObject');
    //varUStrArg  = $004A; {                 74 }
    varUStrArg: Result := TJSONString.Create(VarToStr(AValue));
    //varString   = $0100; { Pascal string  256 } {not OLE compatible }
    varString: Result := TJSONString.Create(VarToStr(AValue));
    //varAny      = $0101; { Corba any      257 } {not OLE compatible }
    varAny: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varAny');
    //varUString  = $0102; { Unicode string 258 } {not OLE compatible }
    varUString: Result := TJSONString.Create(VarToStr(AValue));
    //varByRef    = $4000;
    varByRef: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varByRef');
    //varArray    = $2000;
    varArray: raise EJSONError.Create('Invalid typecast in VariantToJSONValue: varArray');
  else
    raise EJSONError.CreateFmt('Invalid typecast in VariantToJSONValue: $%.4x',
      [VarType(AValue)]);
  end;
end;

function TJSONList.VarRecToJSONValue(const AValue: TVarRec): TJSONValue;
// This method will Translate a VarRec Structure to a JSON Value Object.
begin
  case AValue.VType of
    //vtBoolean       = 1;
    vtBoolean: Result := TJSONBoolean.Create(AValue.VBoolean);
    //vtInt64         = 16;
    vtInt64: Result := TJSONNumber.Create(AValue.VInt64^);
    //vtInteger       = 0;
    vtInteger: Result := TJSONNumber.Create(AValue.VInteger);
    //vtCurrency      = 12;
    vtCurrency: Result := TJSONNumber.Create(AValue.VCurrency^);
    //vtExtended      = 3;
    vtExtended: Result := TJSONNumber.Create(AValue.VExtended^);
    //vtChar          = 2;
    vtChar: Result := TJSONString.Create(string(AValue.VChar));
    //vtPChar         = 6;
    vtPChar: Result := TJSONString.Create(string(AValue.VPChar^));
    //vtString        = 4;
    vtString: Result := TJSONString.Create(string(AValue.VString));
    //vtWideChar      = 9;
    vtWideChar: Result := TJSONString.Create(WideString(AValue.VWideChar));
    //vtPWideChar     = 10;
    vtPWideChar: Result := TJSONString.Create(WideString(AValue.VPWideChar));
    //vtWideString    = 15;
    vtWideString: Result := TJSONString.Create(WideString(AValue.VWideString));
    //vtAnsiString    = 11;
    vtAnsiString: Result := TJSONString.Create(AnsiString(AValue.VAnsiString));
    //vtUnicodeString = 17;
    vtUnicodeString: Result := TJSONString.Create(UnicodeString(AValue.VUnicodeString));
  {$IFDEF AUTOREFCOUNT}
    //vtObject        = 7;
    vtObject: if not TObject(AValue.VObject).InheritsFrom(TJSONValue) then
      Result := TJSONObject.Create(TObject(AValue.VObject)) else
      Result := TJSONObject(AValue.VObject);
  {$ELSE}
    //vtObject        = 7;
    vtObject: if not AValue.VObject.InheritsFrom(TJSONValue) then
      Result := TJSONObject.Create(AValue.VObject) else
      Result := TJSONObject(AValue.VObject);
  {$ENDIF}
    //vtVariant       = 13;
    vtVariant: Result := VariantToJSONValue(AValue.VVariant^);
    //vtPointer       = 5;
    vtPointer: raise EJSONError.Create(
      'Invalid typecast in VarRecToJSONValue: vtPointer');
    //vtClass         = 8;
    vtClass: raise EJSONError.Create('Invalid typecast in VarRecToJSONValue: vtClass');
    //vtInterface     = 14;
    vtInterface: raise EJSONError.Create('Invalid typecast in VarRecToJSONValue: vtInterface');
  else
    raise EJSONError.CreateFmt('Invalid typecast in VarRecToJSONValue: $%.4x',
      [AValue.VType]);
  end;
end;

{==============================================================================}
{ TJSONArray }

procedure TJSONArray.AddArray(const AValue: array of const);
// This method will Add a Array of Const to the Internal Data Structure.
begin
  AddItem(TJSONArray.Create(AValue));
end;

procedure TJSONArray.AddBoolean(const AValue: Boolean);
// This method will Add a Boolean Value to the Array Values.
begin
  AddItem(TJSONBoolean.Create(AValue));
end;

procedure TJSONArray.AddDateTime(const AValue: TDateTime);
// This method will Add a DateTime Value to the Array Values.
begin
  AddItem(TJSONNumber.Create(AValue));
end;

procedure TJSONArray.AddFloat(const AValue: Extended);
// This method will Add a Float Value to the Array Values.
begin
  AddItem(TJSONNumber.Create(AValue));
end;

procedure TJSONArray.AddInteger(const AValue: Int64);
// This method will Add a Integer Value to the Array Values.
begin
  AddItem(TJSONNumber.Create(AValue));
end;

procedure TJSONArray.AddNull;
// This method will Add a Null Value to the Array Values.
begin
  AddItem(TJSONNull.Create);
end;

procedure TJSONArray.AddObject(const AValue: TObject);
// This method will Add a TObject to the Array Values.
begin
  AddItem(TJSONObject.Create(AValue));
end;

procedure TJSONArray.AddString(const AValue: string);
// This method will Add a String Value to the Array Values.
begin
  AddItem(TJSONString.Create(AValue));
end;

procedure TJSONArray.AddValue(const AValue: TJSONValue);
// This method will Add a JSON Value Object to the Array Values.
// NOTE: The List will take ownership of this Object.
begin
  AddItem(AValue);
end;

function TJSONArray.ApplyIndentation(AIndent: Integer): Boolean;
// This method will Return True if intentation should be applied or not..
begin
  Result := inherited ApplyIndentation(AIndent) and
    (EstimatedSize(AIndent) > 60);
end;

function TJSONArray.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
begin
  Result := ComposeList('[]', AIndent);
end;

function TJSONArray.ComposeValueAt(AIndex: Integer; AList: TJSONList;
  AIndent: Integer): string;
// This routine will Compose the JSON string for the given Item.
begin
  if AList[AIndex] = nil then raise EJSONError.CreateFmt(
    'Array Value at Index %s is not assigned on the JSONArray.', [AIndex]);

  Result := inherited ComposeValueAt(AIndex, AList, AIndent) +
    TJSONValue(AList.FList[AIndex]).Compose(AIndent);
end;

constructor TJSONArray.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
end;

constructor TJSONArray.Create(const AValue: array of const);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  ExtractValues(AValue);
end;

class function TJSONArray.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
begin
  if ADataStr = EmptyStr then RaiseParserError(ADataStr,
    AIndex, 'Array', 'Invalid JSON Data Text.');

  Result := DecomposeList('[]', ADataStr, AIndex);
end;

class procedure TJSONArray.DecomposeValueAt(const ADataStr: string;
  var AIndex: Integer; AList: TJSONList);
// This method will Decompose the Value at the given Index.
begin
  AList.AddItem(TJSONValue.Decompose(ADataStr, AIndex));
  inherited DecomposeValueAt(ADataStr, AIndex, AList);
end;

destructor TJSONArray.Destroy;
// This method destructs an object instance.
begin
  inherited Destroy;
end;

function TJSONArray.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
  Result := inherited EstimatedSize;
end;

procedure TJSONArray.ExtractValues(const AValue: array of const);
// This method will Extract the values in AValue to this JSON Object.
var
  Index: Integer;
begin
  for Index := Low(AValue) to High(AValue) do
    AddItem(VarRecToJSONValue(AValue[Index]));

//DelME
//  begin
//    case AValue[Index].VType of
//      vtBoolean: AddItem(TJSONBoolean.Create(AValue[Index].VBoolean));
//
//      vtInt64: AddItem(TJSONNumber.Create(AValue[Index].VInt64^));
//      vtInteger: AddItem(TJSONNumber.Create(AValue[Index].VInteger));
//      vtCurrency: AddItem(TJSONNumber.Create(AValue[Index].VCurrency^));
//      vtExtended: AddItem(TJSONNumber.Create(AValue[Index].VExtended^));
//      vtPointer: if Assigned(AValue[Index].VPointer) then
//        raise EJSONError.CreateFmt('Invalid typecast at Index %d', [Index]) else
//        AddItem(TJSONNull.Create);
//
//      vtChar: AddItem(TJSONString.Create(string(AValue[Index].VChar)));
//      vtPChar: AddItem(TJSONString.Create(string(AValue[Index].VPChar^)));
//      vtString: AddItem(TJSONString.Create(string(AValue[Index].VString)));
//      vtWideChar: AddItem(TJSONString.Create(WideString(AValue[Index].VWideChar)));
//      vtPWideChar: AddItem(TJSONString.Create(WideString(AValue[Index].VPWideChar)));
//      vtWideString: AddItem(TJSONString.Create(WideString(AValue[Index].VWideString)));
//      vtAnsiString: AddItem(TJSONString.Create(AnsiString(AValue[Index].VAnsiString)));
//      vtUnicodeString: AddItem(TJSONString.Create(UnicodeString(AValue[Index].VUnicodeString)));
//    {$IFDEF AUTOREFCOUNT}
//      vtObject: if not TObject(AValue[Index].VObject).InheritsFrom(TJSONValue) then
//        AddItem(TJSONObject.Create(TObject(AValue[Index].VObject))) else
//        AddItem(TJSONObject(AValue[Index].VObject));
//    {$ELSE}
//      vtObject: if not AValue[Index].VObject.InheritsFrom(TJSONValue) then
//        AddItem(TJSONObject.Create(AValue[Index].VObject)) else
//        AddItem(TJSONObject(AValue[Index].VObject));
//    {$ENDIF}
//      vtVariant:
//      begin
//        case VarType(AValue[Index].VVariant^) and VarTypeMask of
//          varBoolean:
//            AddItem(TJSONBoolean.Create(AValue[Index].VVariant^));
//          varSmallInt, varInteger, varInt64, varByte, varWord, varLongWord:
//            AddItem(TJSONNumber.Create(Integer(AValue[Index].VVariant^)));
//          varSingle, varDouble, varCurrency, varDate:
//            AddItem(TJSONNumber.Create(Extended(AValue[Index].VVariant^)));
//          varOleStr, varString, varUString:
//            AddItem(TJSONString.Create(AValue[Index].VVariant^));
//        else
//          raise EJSONError.CreateFmt('Invalid typecast at Index %d', [Index]);
//        end;
//      end
//    else
//      raise EJSONError.CreateFmt('Invalid typecast at Index %d', [Index]);
//    end;
//  end;
end;

function TJSONArray.GetItem(AIndex: Integer): TJSONValue;
// Read spesifier method for the Item property.
// NOTE: The Type Validation is just an extra check for the case where
//  a JSONObject are incorrectly typecast as an JSONArray.
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['index', AIndex]);
  if not(FList[AIndex] is TJSONValue) then raise EJSONError.CreateFmt(
    'Invalid Item Value stored in Array at Index %d', [AIndex]);

  Result := TJSONValue(FList[AIndex]);
end;

procedure TJSONArray.InsertArray(AIndex: Integer; const AValue: array of const);
// This method will Insert a Array Value to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONArray.Create(AValue));
end;

procedure TJSONArray.InsertBoolean(AIndex: Integer; const AValue: Boolean);
// This method will Insert a Boolean Value to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONBoolean.Create(AValue));
end;

procedure TJSONArray.InsertDateTime(AIndex: Integer; const AValue: TDateTime);
// This method will Insert a Date Time Value to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONNumber.Create(AValue));
end;

procedure TJSONArray.InsertFloat(AIndex: Integer; const AValue: Extended);
// This method will Insert a Float Value to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONNumber.Create(AValue));
end;

procedure TJSONArray.InsertInteger(AIndex: Integer; const AValue: Int64);
// This method will Insert a Integer Value to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONNumber.Create(AValue));
end;

procedure TJSONArray.InsertNull(AIndex: Integer);
// This method will Insert a Null Value to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONNull.Create);
end;

procedure TJSONArray.InsertObject(AIndex: Integer; const AValue: TObject);
// This method will Insert a TObject to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONObject.Create(AValue));
end;

procedure TJSONArray.InsertString(AIndex: Integer; const AValue: string);
// This method will Insert a String Value to the Array Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONString.Create(AValue));
end;

procedure TJSONArray.InsertValue(AIndex: Integer; const AValue: TJSONValue);
// This method will Insert a JSON Value Object to the Array Values
// at the given Index.
// NOTE: The List will take ownership of this Object.
begin
  InsertItem(AIndex, AValue);
end;

procedure TJSONArray.SetItem(AIndex: Integer; const AValue: TJSONValue);
// Write spesifier method for the Item property.
// NOTE: The Type Validation is just an extra check for the case where
//  a JSONObject are incorrectly typecast as an JSONArray.
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['index', AIndex]);
  if not(FList[AIndex] is TJSONValue) then raise EJSONError.CreateFmt(
    'Invalid Item Value stored in Array at Index %d', [AIndex]);

  FList[AIndex].DisposeOf;
  FList[AIndex] := AValue;
end;

class function TJSONArray.ValueType: TJSONType;
// This method will Return the JSON Type of the Value stored.
begin
  Result := jtArray;
end;

{==============================================================================}
{ TJSONPair }

function TJSONPair.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
begin
  if AIndent < 0 then
    Result := Format('%s:%s', [FName.Compose, FValue.Compose]) else
    Result := Format('%s: %s', [FName.Compose, FValue.Compose(AIndent)]);
end;

constructor TJSONPair.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
  FName := TJSONString.Create;
  FValue := TJSONNull.Create;
end;

constructor TJSONPair.Create(const AName: string; AValue: TJSONValue);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  Name.ValueAsString := AName;
  Value := AValue;
end;

class function TJSONPair.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
var
  ResultingPair: TJSONPair absolute Result;
begin
  ResultingPair := TJSONPair.Create;
  try
    ResultingPair.Name := TJSONString.Decompose(ADataStr, AIndex);
    ValidateToken(':', ADataStr, AIndex, 'Object-Pair');
    ResultingPair.Value := TJSONValue.Decompose(ADataStr, AIndex);
  except
    ResultingPair.DisposeOf;
    raise;
  end;
end;

destructor TJSONPair.Destroy;
// This method destructs an object instance.
begin
  DisposeAndNil(FValue);
  DisposeAndNil(FName);
  inherited Destroy;
end;

function TJSONPair.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
  Result := 2 + FName.EstimatedSize + FValue.EstimatedSize;
end;

procedure TJSONPair.SetName(const AValue: TJSONValue);
// Write spesifier method for the Name property.
begin
  if not(AValue is TJSONString) then raise EJSONError.Create(
    'TJSONPair.Name has to be of type TJSONString.');

  if Assigned(FName) then Name.DisposeOf;
  FName := AValue;
  if not Assigned(FName) then
    FName := TJSONString.Create;
end;

procedure TJSONPair.SetValue(const AValue: TJSONValue);
// Write spesifier method for the Value property.
begin
  if Assigned(FValue) then FValue.DisposeOf;
  FValue := AValue;
  if not Assigned(FValue) then
    FValue := TJSONNull.Create;
end;

{==============================================================================}
{ TJSONObject }

procedure TJSONObject.AddArray(AName: string; const AValue: array of const);
// This method will Add a Array Value to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONArray.Create(AValue)));
end;

procedure TJSONObject.AddBoolean(AName: string; const AValue: Boolean);
// This method will Add a Boolean Value to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONBoolean.Create(AValue)));
end;

procedure TJSONObject.AddDateTime(AName: string; const AValue: TDateTime);
// This method will Add a DateTime Value to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONNumber.Create(AValue)));
end;

procedure TJSONObject.AddFloat(AName: string; const AValue: Extended);
// This method will Add a Float Value to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONNumber.Create(AValue)));
end;

procedure TJSONObject.AddIndex(const AName: string; AIndex: Integer);
// This method will Add an Index to the Internal Hashed List.
var
  BucketIndex: Integer;
  HashBucket: TStringList;
begin
  BucketIndex := CalcHashBucketIndex(AName);
  HashBucket := TStringList(FHashBuckets[BucketIndex]);
  HashBucket.AddObject(AName, TObject(AIndex));
end;

procedure TJSONObject.AddInteger(AName: string; const AValue: Int64);
// This method will Add a Integer Value to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONNumber.Create(AValue)));
end;

function TJSONObject.AddItem(AItem: TJSONCore): Integer;
// This method will Add a JSONValue Item to the Internal Data Structure.
var
  ValuePair: TJSONPair absolute AItem;
begin
  if not(AItem is TJSONPair) then raise EJSONError.Create(
    'Invalid Pair Value added to Object.');

  Result := inherited AddItem(AItem);
  if FValidIndex then AddIndex(ValuePair.Name.ValueAsString, Result);
end;

procedure TJSONObject.AddNull(AName: string);
// This method will Add a Null Value to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONNull.Create));
end;

procedure TJSONObject.AddObject(AName: string; const AValue: TObject);
// This method will Add a TObject to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONObject.Create(AValue)));
end;

procedure TJSONObject.AddString(AName: string; const AValue: string);
// This method will Add a String Value to the Object Values.
begin
  AddItem(TJSONPair.Create(AName, TJSONString.Create(AValue)));
end;

procedure TJSONObject.AddValue(AName: string; const AValue: TJSONValue);
// This method will Add a JSON Value Object to the Object Values.
// NOTE: The List will take ownership of this Object.
begin
  AddItem(TJSONPair.Create(AName, AValue));
end;

function TJSONObject.CalcHashBucketIndex(const AName: string): Cardinal;
// This method will Calculate the Hash Index of AName.
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to AName.Length - 1 do
    Result := ((Result shl 2) or (Result shr
      (SizeOf(Result) * 8 - 2))) xor Ord(AName.Chars[Index]);
  Result := Result mod Cardinal(CMaxHashBuckets);
end;

procedure TJSONObject.Clear;
// This method will Clear the Internal Data Structure.
begin
  inherited Clear;
  ClearIndex;
end;

procedure TJSONObject.ClearIndex;
// This method will Clear the Internal Hashed List.
var
  Index: Integer;
begin
  FValidIndex := True;
  for Index := Low(FHashBuckets) to High(FHashBuckets) do
    FHashBuckets[Index].Clear;
end;

function TJSONObject.Compose(AIndent: Integer): string;
// This routine will Compose the JSON string.
begin
  Result := ComposeList('{}', AIndent);
end;

function TJSONObject.ComposeValueAt(AIndex: Integer; AList: TJSONList;
  AIndent: Integer): string;
// This routine will Compose the JSON string for the given Item.
begin
  if AList[AIndex] = nil then raise EJSONError.CreateFmt(
    'Member Value at Index %s is not assigned on the JSONObject.', [AIndex]);

  Result := inherited ComposeValueAt(AIndex, AList, AIndent) +
    TJSONPair(AList.FList[AIndex]).Compose(AIndent);
end;

constructor TJSONObject.Create;
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  inherited Create;
  InitializeIndex;
end;

constructor TJSONObject.Create(const AObjValue: TObject);
// This method constructs an object and initializes it's data before the
// object is first used.
begin
  Create;
  ImportFromObject(AObjValue);
end;

class function TJSONObject.Decompose(const ADataStr: string;
  var AIndex: Integer): TJSONValue;
// This routine will Decompose the JSON string and return a
// JSON Object containing the Value.
begin
  if ADataStr = EmptyStr then RaiseParserError(ADataStr,
    AIndex, 'Object', 'Invalid JSON Data Text.');

  Result := DecomposeList('{}', ADataStr, AIndex);
end;

class procedure TJSONObject.DecomposeValueAt(const ADataStr: string;
  var AIndex: Integer; AList: TJSONList);
// This method will Decompose the Value at the given Index.
begin
  AList.AddItem(TJSONPair.Decompose(ADataStr, AIndex));
  inherited DecomposeValueAt(ADataStr, AIndex, AList);
end;

procedure TJSONObject.DeleteItem(AIndex: Integer);
// This method will Delete a JSONValue Item from the Internal Data Structure.
begin
  inherited DeleteItem(AIndex);
  FValidIndex := FValidIndex and (AIndex >= Count);
end;

destructor TJSONObject.Destroy;
// This method destructs an object instance.
begin
  FinalizeIndex;
  inherited Destroy;
end;

function TJSONObject.EstimatedSize(AIndent: Integer): Integer;
// This method will Return the maximum estimated Text Buffer Size required to
// store the JSON Object.
begin
  Result := inherited EstimatedSize;
end;

procedure TJSONObject.ExportToObject(AObject: TObject);
// This method will copy the values from this JSON Object Structure into the given AObject instance.
var
  LContext: TRttiContext;
  LType: TRttiType;
  //LMethod: TRttiMethod;
  LProperty: TRttiProperty;
  //LField: TRttiField;
  LJSONValue: TJSONValue;
  LNewValue: TValue;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(AObject.ClassType);

    for LProperty in LType.GetProperties do
    begin
      if LProperty.IsWritable and FindMemberValue(LProperty.Name, LJSONValue) then
      begin
        case LJSONValue.ValueType of
          // Unknown Error Value
          jtUnknown: LNewValue := nil;
          // NULL Value: "null"
          jtNull: LNewValue := nil;
          // Boolean Value: "true" or "false"
          jtBoolean: LNewValue := LJSONValue.ValueAsBoolean;
          // Numeric Value: 0..9
          jtNumber: LNewValue := LJSONValue.ValueAsFloat;
          // String: Value: "..."
          jtString: LNewValue := LJSONValue.ValueAsString;
          // Array Value: [...]
          jtArray: raise EJSONConvertError.Create('Not implemented yet ...');
          // Object Value: {...}
          jtObject: raise EJSONConvertError.Create('Not implemented yet ...');
        else
          raise EJSONConvertError.Create('Not implemented yet ...');
        end;
        LProperty.SetValue(AObject, LNewValue);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure TJSONObject.ExtractValues(const AObjValue: TObject);
// This method will Extract the values in AValue to this JSON Object.
var
  RttiContext: TRttiContext;
  RttiProperty: TRttiProperty;
  RttiType: TRttiType;
begin
  Assert(False, 'Depricated call to TJSONObject.ExtractValues');
  RttiContext := TRttiContext.Create.Create;
  try
    RttiType := RttiContext.GetType(AObjValue.ClassInfo);
    for RttiProperty in RttiType.GetProperties do
    begin
      with RttiProperty do
      begin
        if IsReadable and IsWritable and (Visibility in [mvPublished, mvPublic]) then
        try
          AddValue(Name, RTTIValueToJSONValue(GetValue(AObjValue)));
        except
          on E: Exception do AddString(Name, Format(
            'Error adding %s value for %s.'+ sLineBreak +
            'Error: %s', [
              GetEnumName(TypeInfo(TTypeKind), Ord(PropertyType.TypeKind)),
              Name, E.Message]));
        end;
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

procedure TJSONObject.FinalizeIndex;
// This method will Finalize the Internal Hashed List.
var
  Index: Integer;
begin
  for Index := Low(FHashBuckets) to High(FHashBuckets) do
    FHashBuckets[Index].DisposeOf;
  SetLength(FHashBuckets, 0);
end;

function TJSONObject.FindIndex(const AName: string; var AIndex: Integer): Boolean;
// This method will Find an Index in the Internal Hashed List.
var
  HashIndex: Integer;
  HashBucket: TStringList;
  HashBucketIndex: Cardinal;
begin
  HashBucketIndex := CalcHashBucketIndex(AName);
  HashBucket := TStringList(FHashBuckets[HashBucketIndex]);
  Result := HashBucket.Find(AName, HashIndex);
  if Result then
    AIndex := Integer(HashBucket.Objects[HashIndex]) else
    AIndex := -1;
end;

function TJSONObject.FindMemberValue(AName: string;
  var AValue: TJSONValue): Boolean;
// This method will Find the Member with the given Name, if it exists and
// return the Value for it.
var
  Index: Integer;
begin
  if not FValidIndex then Reindex;
  Result := FindIndex(AName, Index);
  if Result then
    AValue := GetItem(Index) else
    AValue := nil;
end;

function TJSONObject.GetItem(AIndex: Integer): TJSONValue;
// Read spesifier method for the Item property.
// NOTE: The Type Validation is just an extra check for the case where
//  a JSONArray are incorrectly typecast as an JSONObject.
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['index', AIndex]);
  if not(FList[AIndex] is TJSONPair) then raise EJSONError.CreateFmt(
    'Invalid Pair Value stored in Object at Index %d', [AIndex]);

  Result := TJSONPair(FList[AIndex]).Value;
end;

function TJSONObject.GetMember(AName: string): TJSONValue;
// Read spesifier method for the Member property.
var
  LIndex: Integer;
begin
  if not FValidIndex then Reindex;
  if FindIndex(AName, LIndex) then
    Result := GetItem(LIndex) else
    Result := nil;
end;

function TJSONObject.GetMemberName(AIndex: Integer): string;
// Read spesifier method for the MemberName property.
// NOTE: The Type Validation is just an extra check for the case where
//  a JSONArray are incorrectly typecast as an JSONObject.
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['index', AIndex]);
  if not(FList[AIndex] is TJSONPair) then raise EJSONError.CreateFmt(
    'Invalid Pair Value stored in Object at Index %d', [AIndex]);

  Result := TJSONPair(FList[AIndex]).Name.ValueAsString;
end;

procedure TJSONObject.ImportFromObject(AObject: TObject);
// This method will copy the values into this JSON Object Structure from the given AObject instance.
var
  RttiType: TRttiType;
//  RttiFields: TRttiField;
  RttiProperty: TRttiProperty;
  RttiContext: TRttiContext;
begin
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(AObject.ClassInfo);
    for RttiProperty in RttiType.GetProperties do
    begin
      with RttiProperty do
      begin
        if IsReadable and IsWritable and (Visibility in [mvPublished, mvPublic]) then
        try
          AddValue(Name, RTTIValueToJSONValue(GetValue(AObject)));
        except
          on E: EJSONConvertError do AddString(Name, Format(
            'Error adding %s value for %s.'+ sLineBreak +
            'Error: %s', [
              GetEnumName(TypeInfo(TTypeKind), Ord(PropertyType.TypeKind)),
              Name, E.Message]));
        end;
      end;
    end;
//DelME
//    for RttiFields in RttiType.GetFields do
//    begin
//      // Add code here ...
//    end;
  finally
    RttiContext.Free;
  end;
end;

procedure TJSONObject.InitializeIndex;
// This method will Initialize the Internal Hashed List.
var
  Index: Integer;
begin
  SetLength(FHashBuckets, CMaxHashBuckets);
  for Index := Low(FHashBuckets) to High(FHashBuckets) do
  begin
    FHashBuckets[Index] := TStringList.Create;
    FHashBuckets[Index].Duplicates := dupIgnore;
    FHashBuckets[Index].Sorted := True;
  end;
end;

procedure TJSONObject.InsertArray(AIndex: Integer; AName: string;
  const AValue: array of const);
// This method will Insert a Array Value to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONArray.Create(AValue)));
end;

procedure TJSONObject.InsertBoolean(AIndex: Integer; AName: string;
  const AValue: Boolean);
// This method will Insert a Boolean Value to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONBoolean.Create(AValue)));
end;

procedure TJSONObject.InsertDateTime(AIndex: Integer; AName: string;
  const AValue: TDateTime);
// This method will Insert a DateTime Value to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONNumber.Create(AValue)));
end;

procedure TJSONObject.InsertFloat(AIndex: Integer; AName: string;
  const AValue: Extended);
// This method will Insert a Float Value to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONNumber.Create(AValue)));
end;

procedure TJSONObject.InsertInteger(AIndex: Integer; AName: string;
  const AValue: Int64);
// This method will Insert a Integer Value to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONNumber.Create(AValue)));
end;

procedure TJSONObject.InsertItem(AIndex: Integer; AItem: TJSONCore);
// This method will Insert a JSONValue Item to the Internal Data Structure
// at the given AIndex.
begin
  if not(AItem is TJSONPair) then raise EJSONError.Create(
    'Invalid Pair Value added to Object.');

  inherited InsertItem(AIndex, AItem);
  FValidIndex := False;
end;

procedure TJSONObject.InsertNull(AIndex: Integer;
  AName: string);
// This method will Insert a Null Value to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONNull.Create));
end;

procedure TJSONObject.InsertObject(AIndex: Integer; AName: string;
  const AValue: TObject);
// This method will Insert a TObject to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONObject.Create(AValue)));
end;

procedure TJSONObject.InsertString(AIndex: Integer; AName: string;
  const AValue: string);
// This method will Insert a String Value to the Object Values
// at the given Index.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, TJSONString.Create(AValue)));
end;

procedure TJSONObject.InsertValue(AIndex: Integer; AName: string;
  const AValue: TJSONValue);
// This method will Insert a JSON Value Object to the Object Values
// at the given Index.
// NOTE: The List will take ownership of this Object.
begin
  InsertItem(AIndex, TJSONPair.Create(AName, AValue));
end;

procedure TJSONObject.Reindex;
// This method will Re-Index the Internal Hashed List.
var
  Index: Integer;
  Item: TJSONPair;
begin
  ClearIndex;
  for Index := 0 to FCount-1 do
  begin
    Item := TJSONPair(FList[Index]);
    AddIndex(Item.Name.ValueAsString, Index);
  end;
end;

procedure TJSONObject.SetItem(AIndex: Integer; const AValue: TJSONValue);
// Write spesifier method for the Item property.
// NOTE: The Type Validation is just an extra check for the case where
//  a JSONArray are incorrectly typecast as an JSONObject.
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EJSONError.CreateFmt(SJSONErrOutOfBounds, ['index', AIndex]);
  if not(FList[AIndex] is TJSONPair) then raise EJSONError.CreateFmt(
    'Invalid Pair Value stored in Object at Index %d', [AIndex]);

  TJSONPair(FList[AIndex]).Value := AValue;
end;

procedure TJSONObject.SetMember(AName: string; const AValue: TJSONValue);
// Write spesifier method for the Member property.
var
  LIndex: Integer;
begin
  if not FValidIndex then Reindex;
  if FindIndex(AName, LIndex) then
    SetItem(LIndex, AValue);
end;

class function TJSONObject.ValueType: TJSONType;
// This method will Return the JSON Type of the Value stored.
begin
  Result := jtObject;
end;

end.

