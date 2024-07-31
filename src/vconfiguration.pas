unit vconfiguration;
{$mode ObjFPC}
interface

uses vgenerics;

type TConfigurationEntry      = class;
     TConfigurationGroup      = class;
     TConfigurationManager    = class;
     TConfigurationEntryArray = specialize TGArray< TConfigurationEntry >;
     TConfigurationGroupArray = specialize TGArray< TConfigurationGroup >;
     TConfigurationEntryMap   = specialize TGHashMap< TConfigurationEntry >;
     TConfigurationGroupMap   = specialize TGHashMap< TConfigurationGroup >;
     TConfigurationValueMap   = specialize TGHashMap< Variant >;

type TConfigurationEntry = class
  constructor Create( aID : Ansistring );
  function SetName( aName : Ansistring ) : TConfigurationEntry;
  function SetDescription( aDesc : Ansistring ) : TConfigurationEntry;
protected
  FID   : Ansistring;
  FName : Ansistring;
  FDesc : Ansistring;
public
  property ID          : Ansistring read FID;
  property Name        : Ansistring read FName;
  property Description : Ansistring read FDesc;
end;

type TToggleConfigurationEntry = class( TConfigurationEntry )
  constructor Create( aID : Ansistring; aDefault : Boolean );
  function Access : PBoolean;
protected
  FDefault : Boolean;
  FValue   : Boolean;
public
  property Default : Boolean read FDefault;
  property Value   : Boolean read FValue write FValue;
end;

type TIntegerConfigurationEntry = class( TConfigurationEntry )
  constructor Create( aID : Ansistring; aDefault : Integer );
  function SetRange( aMin, aMax : Integer; aStep : Integer = 1 ) : TIntegerConfigurationEntry;
  function Access : PInteger;
protected
  FDefault : Integer;
  FMin     : Integer;
  FMax     : Integer;
  FStep    : Integer;
  FValue   : Integer;
public
  property Default : Integer read FDefault;
  property Min     : Integer read FMin;
  property Max     : Integer read FMax;
  property Step    : Integer read FStep;
  property Value   : Integer read FValue write FValue;
end;

type TConfigurationGroup = class
  constructor Create( aConfigurationManager : TConfigurationManager );
  function AddInteger( aEntryID : Ansistring; aDefault : Integer ) : TIntegerConfigurationEntry;
  function AddToggle( aEntryID : Ansistring; aDefault : Boolean ) : TToggleConfigurationEntry;
  destructor Destroy; override;
protected
  FManager : TConfigurationManager;
  FEntries : TConfigurationEntryArray;
  FLookup  : TConfigurationEntryMap;
public
  property Entries : TConfigurationEntryArray read FEntries;
end;

type TConfigurationManager = class
  constructor Create;
  function AddGroup( aGroupID : AnsiString ) : TConfigurationGroup;
  function GetInteger( aEntryID : Ansistring ) : Integer;
  function GetBoolean( aEntryID : Ansistring ) : Boolean;
  function AccessInteger( aEntryID : Ansistring ) : PInteger;
  function AccessBoolean( aEntryID : Ansistring ) : PBoolean;
  destructor Destroy; override;
protected
  function CastInteger( aEntryID : Ansistring ) : TIntegerConfigurationEntry;
  function CastBoolean( aEntryID : Ansistring ) : TToggleConfigurationEntry;
  procedure AddEntry( aEntryID : Ansistring; aEntry : TConfigurationEntry );
protected
  FGroups : TConfigurationGroupArray;
  FGroup  : TConfigurationGroupMap;
  FLookup : TConfigurationEntryMap;
public
  property Groups : TConfigurationGroupArray read FGroups;
end;

implementation

uses sysutils;

{ TConfigurationEntry }

constructor TConfigurationEntry.Create( aID: Ansistring );
begin
  FID   := aID;
  FName := '';
  FDesc := '';
end;

function TConfigurationEntry.SetName( aName: Ansistring ): TConfigurationEntry;
begin
  FName  := aName;
  Result := Self;
end;

function TConfigurationEntry.SetDescription( aDesc : Ansistring ): TConfigurationEntry;
begin
  FDesc  := aDesc;
  Result := Self;
end;

{ TToggleConfigurationEntry }

constructor TToggleConfigurationEntry.Create( aID: Ansistring; aDefault: Boolean );
begin
  inherited Create( aID );
  FDefault := aDefault;
end;

function TToggleConfigurationEntry.Access : PBoolean;
begin
  Result := @FValue;
end;

{ TIntegerConfigurationEntry }

constructor TIntegerConfigurationEntry.Create( aID: Ansistring; aDefault: Integer );
begin
  inherited Create( aID );
  FDefault := aDefault;
  FValue   := aDefault;
end;

function TIntegerConfigurationEntry.SetRange( aMin, aMax: Integer; aStep: Integer ): TIntegerConfigurationEntry;
begin
  FMin   := aMin;
  FMax   := aMax;
  FStep  := aStep;
  Result := Self;
end;

function TIntegerConfigurationEntry.Access : PInteger;
begin
  Result := @FValue;
end;

{ TConfigurationGroup }

constructor TConfigurationGroup.Create( aConfigurationManager : TConfigurationManager );
begin
  FManager := aConfigurationManager;
  FEntries := TConfigurationEntryArray.Create;
  FLookup  := TConfigurationEntryMap.Create;
end;

destructor TConfigurationGroup.Destroy;
var iEntry : TConfigurationEntry;
begin
  for iEntry in FEntries do
    iEntry.Free;
  FreeAndNil( FEntries );
  FreeAndNil( FLookup );
  inherited Destroy;
end;

function TConfigurationGroup.AddInteger( aEntryID : Ansistring; aDefault : Integer ): TIntegerConfigurationEntry;
var iEntry : TIntegerConfigurationEntry;
begin
  iEntry := TIntegerConfigurationEntry.Create( aEntryID, aDefault );
  FEntries.Push( iEntry );
  FLookup[ aEntryID ] := iEntry;
  Result := iEntry;
end;

function TConfigurationGroup.AddToggle( aEntryID: Ansistring; aDefault: Boolean ): TToggleConfigurationEntry;
var iEntry : TToggleConfigurationEntry;
begin
  iEntry := TToggleConfigurationEntry.Create( aEntryID, aDefault );
  FEntries.Push( iEntry );
  FLookup[ aEntryID ] := iEntry;
  Result := iEntry;
end;

{ TConfigurationManager }

constructor TConfigurationManager.Create;
begin
  FGroups := TConfigurationGroupArray.Create;
  FGroup  := TConfigurationGroupMap.Create;
  FLookup := TConfigurationEntryMap.Create;
end;

function TConfigurationManager.AddGroup( aGroupID : AnsiString ): TConfigurationGroup;
var iGroup : TConfigurationGroup;
begin
  iGroup := TConfigurationGroup.Create( Self );
  FGroups.Push( iGroup );
  FGroup[aGroupID] := iGroup;
  Result := iGroup;
end;

procedure TConfigurationManager.AddEntry( aEntryID : Ansistring; aEntry : TConfigurationEntry );
begin
  if FLookup.Exists( aEntryID ) then
    raise Exception.Create( 'TConfigurationManager - Entry '+aEntryID+' is duplicated!' );
  FLookup[ aEntryID ] := aEntry;
end;

function TConfigurationManager.GetInteger( aEntryID: Ansistring ): Integer;
begin
  Result := CastInteger( aEntryID ).Value;
end;

function TConfigurationManager.GetBoolean(aEntryID: Ansistring): Boolean;
begin
  Result := CastBoolean( aEntryID ).Value;
end;

function TConfigurationManager.AccessInteger(aEntryID: Ansistring): PInteger;
begin
  Result := CastInteger( aEntryID ).Access;
end;

function TConfigurationManager.AccessBoolean(aEntryID: Ansistring): PBoolean;
begin
  Result := CastBoolean( aEntryID ).Access;
end;

function TConfigurationManager.CastInteger( aEntryID : Ansistring ) : TIntegerConfigurationEntry;
var iLookup : TConfigurationEntry;
begin
  iLookup := FLookup[ aEntryID ];
  if iLookup = nil then raise Exception.Create( 'TConfigurationManager - Entry '+aEntryID+' is not defined!' );
  if not ( iLookup is TIntegerConfigurationEntry ) then
    raise Exception.Create( 'TConfigurationManager - Entry '+aEntryID+' is not a TIntegerConfigurationEntry!' );
  Result := TIntegerConfigurationEntry( iLookup );
end;

function TConfigurationManager.CastBoolean( aEntryID : Ansistring ) : TToggleConfigurationEntry;
var iLookup : TConfigurationEntry;
begin
  iLookup := FLookup[ aEntryID ];
  if iLookup = nil then raise Exception.Create( 'TConfigurationManager - Entry '+aEntryID+' is not defined!' );
  if not ( iLookup is TToggleConfigurationEntry ) then
    raise Exception.Create( 'TConfigurationManager - Entry '+aEntryID+' is not a TToggleConfigurationEntry!' );
  Result := TToggleConfigurationEntry( iLookup );
end;

destructor TConfigurationManager.Destroy;
var iEntry : TConfigurationGroup;
begin
  for iEntry in FGroups do
    iEntry.Free;
  FreeAndNil( FGroups );
  FreeAndNil( FGroup );
  FreeAndNil( FLookup );
  inherited Destroy;
end;

end.

