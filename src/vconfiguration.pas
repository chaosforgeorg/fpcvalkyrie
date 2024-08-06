unit vconfiguration;
{$mode ObjFPC}
interface

uses vnode, vgenerics, vlualibrary;

type TConfigurationEntry      = class;
     TConfigurationGroup      = class;
     TConfigurationManager    = class;
     TConfigurationEntryArray = specialize TGArray< TConfigurationEntry >;
     TConfigurationGroupArray = specialize TGArray< TConfigurationGroup >;
     TConfigurationEntryMap   = specialize TGHashMap< TConfigurationEntry >;
     TConfigurationGroupMap   = specialize TGHashMap< TConfigurationGroup >;
     TConfigurationValueMap   = specialize TGHashMap< Variant >;

type TConfigurationEntry = class( TVObject )
  constructor Create( aID : Ansistring );
  function SetName( aName : Ansistring ) : TConfigurationEntry;
  function SetDescription( aDesc : Ansistring ) : TConfigurationEntry;
  procedure Reset; virtual; abstract;
protected
  function ToLuaString : Ansistring; virtual; abstract;
  function ParseValue( aState : PLua_State; aIndex : Integer ) : Boolean; virtual; abstract;
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
  procedure Reset; override;
protected
  function ToLuaString : Ansistring; override;
  function ParseValue( aState : PLua_State; aIndex : Integer ) : Boolean; override;
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
  procedure Reset; override;
protected
  function ToLuaString : Ansistring; override;
  function ParseValue( aState : PLua_State; aIndex : Integer ) : Boolean; override;
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

type TConfigurationGroup = class( TVObject )
  constructor Create( aConfigurationManager : TConfigurationManager );
  function AddInteger( aEntryID : Ansistring; aDefault : Integer ) : TIntegerConfigurationEntry;
  function AddToggle( aEntryID : Ansistring; aDefault : Boolean ) : TToggleConfigurationEntry;
  destructor Destroy; override;
protected
  procedure AddEntry( aEntryID : Ansistring; aEntry : TConfigurationEntry );
protected
  FManager : TConfigurationManager;
  FEntries : TConfigurationEntryArray;
  FLookup  : TConfigurationEntryMap;
public
  property Entries : TConfigurationEntryArray read FEntries;
end;

type TConfigurationManager = class( TVObject )
  constructor Create;
  function GetInteger( aEntryID : Ansistring ) : Integer;
  function GetBoolean( aEntryID : Ansistring ) : Boolean;
  function AccessInteger( aEntryID : Ansistring ) : PInteger;
  function AccessBoolean( aEntryID : Ansistring ) : PBoolean;
  function CastInteger( aEntryID : Ansistring ) : TIntegerConfigurationEntry;
  function CastBoolean( aEntryID : Ansistring ) : TToggleConfigurationEntry;
  function Read( aFileName : Ansistring ) : Boolean;
  function Write( aFileName : Ansistring ) : Boolean;
  destructor Destroy; override;
protected
  function AddGroup( aGroupID : AnsiString ) : TConfigurationGroup;
protected
  procedure AddEntry( aEntryID : Ansistring; aEntry : TConfigurationEntry );
protected
  FGroups : TConfigurationGroupArray;
  FGroup  : TConfigurationGroupMap;
  FLookup : TConfigurationEntryMap;
public
  property Group  : TConfigurationGroupMap   read FGroup;
  property Groups : TConfigurationGroupArray read FGroups;
end;

implementation

uses sysutils, vutil;

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

procedure TToggleConfigurationEntry.Reset;
begin
  FValue := FDefault;
end;

function TToggleConfigurationEntry.ToLuaString : Ansistring;
begin
  if FValue
    then Result := 'true'
    else Result := 'false';
end;

function TToggleConfigurationEntry.ParseValue( aState : PLua_State; aIndex : Integer ) : Boolean;
begin
  Result := True;
  if lua_type( aState, aIndex ) = LUA_TBOOLEAN
    then FValue := lua_toboolean( aState, aIndex )
    else begin Log( LOGWARN, 'Malformed entry "'+FID+'", boolean expected!' ); Result := False; end;
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

procedure TIntegerConfigurationEntry.Reset;
begin
  FValue := FDefault;
end;

function TIntegerConfigurationEntry.ToLuaString : Ansistring;
begin
  Result := IntToStr( FValue );
end;

function TIntegerConfigurationEntry.ParseValue( aState : PLua_State; aIndex : Integer ) : Boolean;
begin
  Result := True;
  if lua_type( aState, aIndex ) = LUA_TNUMBER
    then FValue := lua_tointeger( aState, aIndex )
    else begin Log( LOGWARN, 'Malformed entry "'+FID+'", integer expected!' ); Result := False; end;
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
begin
  Result := TIntegerConfigurationEntry.Create( aEntryID, aDefault );
  AddEntry( aEntryID, Result );
end;

function TConfigurationGroup.AddToggle( aEntryID: Ansistring; aDefault: Boolean ): TToggleConfigurationEntry;
begin
  Result := TToggleConfigurationEntry.Create( aEntryID, aDefault );
  AddEntry( aEntryID, Result );
end;

procedure TConfigurationGroup.AddEntry( aEntryID : Ansistring; aEntry : TConfigurationEntry );
begin
  FEntries.Push( aEntry );
  FLookup[ aEntryID ] := aEntry;
  FManager.AddEntry( aEntryID, aEntry );
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

function TConfigurationManager.Read( aFileName : Ansistring ) : Boolean;
var iText  : Text;
    iState : PLua_State;
    iKey   : Ansistring;
    iEntry : TConfigurationEntry;
begin
  Log( 'Reading configuration from '+aFileName+' ...');
  if not LoadLua then
  begin
    Log( LOGERROR, 'Could not load Lua!' );
    Exit( False );
  end;
  iState := lua_open;
  if iState = nil then
  begin
    Log( LOGERROR, 'Could not create Lua state!' );
    Exit( False );
  end;
  try
    if luaL_dofile( iState, PChar( aFileName ) ) <> 0 then
    begin
      Log( LOGERROR, 'Could not load file into Lua : '+lua_tostring(iState,-1) );
      Exit( False );
    end;

    lua_getglobal( iState, 'configuration' );
    lua_pushnil( iState );
    while ( lua_next( iState, -2 ) <> 0 ) do
    begin
      if lua_type( iState, -2 ) <> LUA_TSTRING then
      begin
        Log( LOGERROR, 'Malformed configuration file!' );
        Exit( False );
      end;
      iKey := lua_tostring( iState, -2 );
      iEntry := FLookup[ iKey ];
      if iEntry = nil
        then Log( LOGWARN, 'Unknown key in configuration file: '+ iKey )
        else iEntry.ParseValue( iState, -1 );
      lua_pop(iState, 1);
    end;

    Log( 'Configuration read from '+aFileName+' successfully.' );
  finally
    lua_close(iState);
  end;
end;

function TConfigurationManager.Write( aFileName : Ansistring ) : Boolean;
var iText  : Text;
    iGroup : TConfigurationGroup;
    iEntry : TConfigurationEntry;
begin
  Result := True;
  Log( 'Writing configuration to '+aFileName+' ...');

  AssignFile( iText, aFileName );
  try
    Rewrite( iText );
    Writeln( iText, '-- this file is auto-generated, edit at own risk' );
    Writeln( iText, 'configuration = {' );
    for iGroup in FGroups do
      for iEntry in iGroup.FEntries do
        Writeln( iText, '  '+iEntry.ID+ ' = ' +iEntry.ToLuaString + ',' );
    Writeln( iText, '}' );
  except
    on E: Exception do
    begin
      Log( LOGERROR, 'An unexpected error occurred: ' + E.Message );
      Result := False;
    end;
  end;

  try
    CloseFile( iText );
    Log( 'Configuration saved to '+aFileName );
  except
    on E: Exception do
    begin
      Log( LOGERROR, 'Error closing the file: ' + E.Message );
      Result := False;
    end;
  end;

end;

end.

