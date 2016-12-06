unit vluaconfig;
{$mode objfpc}
interface

uses vnode, vluastate, vioevent, viotypes;

type TEntryCallback = procedure ( key, value : Variant ) of object;


const COMMAND_INVALID = 255;

type

{ TLuaConfig }

TLuaConfig = class(TVObject)
    constructor Create( const aFileName : Ansistring = ''; aState : PLua_State = nil );
    procedure LoadKeybindings( const aTableName : AnsiString = '' );
    function GetKeybinding ( aCommand : Byte ) : AnsiString;
    function GetKeyCode ( aCommand : Byte ) : TIOKeyCode;
    function RunKey( const aKeyID : AnsiString ) : Variant;
    function RunKey( aKeyCode : TIOKeyCode ) : Variant;
    procedure Load( const aFileName : Ansistring );
    procedure LoadMain( const aFileName : Ansistring );
    procedure EntryFeed( const Table : AnsiString; const Callback : TEntryCallback );
    procedure RecEntryFeed( const Table : AnsiString; const Callback : TEntryCallback );
    procedure SetConstant( const ID : AnsiString; const Value : Variant );
    function Call(const Path: array of const; const Args: array of const): Variant;
    function Configure( const ID : AnsiString; aDefault : Variant ) : Variant;
    function isPaused : Boolean;
    function Resume : Variant;
    procedure ResetCommands;
    destructor Destroy; override;
  protected
    procedure CommandCallback( key, value : Variant ); virtual;
    function GetValue( const Key : AnsiString ) : Variant;
    function HasValue( const Key : AnsiString ) : Boolean;
    function Resolve( const Key : AnsiString ) : Boolean;
    function GetCommand( Key : TIOKeyCode ) : Byte;
    procedure SetCommand( Key : TIOKeyCode; Value : Byte );
  protected
    FState      : PLua_State;
    FThread     : PLua_State;
    FLuaState   : TLuaState;
    FResult     : Variant;
    FKeyTabName : AnsiString;
    FConfigPath : AnsiString;
    FCommands   : array[0..IOKeyCodeMax] of Byte;
  public
    property ConfigPath : AnsiString read FConfigPath write FConfigPath;
    property Commands[ const Key : TIOKeyCode ] : Byte read GetCommand write SetCommand;
    property Entries[ const Key : AnsiString ] : Variant read GetValue; default;
    property Valid[ const Key : AnsiString ] : Boolean read HasValue;
    property Raw : PLua_State read FState;
    property State : TLuaState read FLuaState;
  end;

implementation

uses sysutils, strutils, variants, vdebug, vlualibrary, vluaext, vluatype;

function lua_config_dofile( L: Plua_State ) : Integer; cdecl;
var iFileName  : AnsiString;
    iFullName  : AnsiString;
    iRootPath  : AnsiString;
begin
  if lua_gettop(L) <> 1 then luaL_error( L, 'Require has wrong amount of parameters!');

  iFileName := lua_tostring( L, 1 );
  lua_getglobal( L, '__rootpath' );
  iRootPath := lua_tostring( L, -1 );
  iFullName := iRootPath+iFileName;
  lua_pop( L, 1 );

  if luaL_dofile(L, PChar(iFullName)) <> 0 then
    luaL_error( L, 'require "%s" failed!',PChar(iFullName));
  Exit( 0 );
end;

constructor TLuaConfig.Create( const aFileName : Ansistring = ''; aState : PLua_State = nil);
begin
  FConfigPath := '';
  ResetCommands;
  FKeyTabName := 'keybindings';
  if aState = nil then
  begin
    LoadLua;
    FState := lua_open();
    luaopen_base( FState );
    luaopen_string( FState );
    luaopen_table( FState );
    luaopen_math( FState );
  end
  else
    FState := aState;

  if aFileName <> '' then LoadMain( aFileName );
  FThread := nil;
  FResult := Null;
  FLuaState.Init( FState );
end;

procedure TLuaConfig.LoadKeybindings ( const aTableName : AnsiString ) ;
begin
  if aTableName <> '' then FKeyTabName := aTableName;
  EntryFeed(FKeyTabName, @CommandCallback );
end;

function TLuaConfig.GetKeybinding ( aCommand : Byte ) : AnsiString;
var iCount : Word;
begin
  for iCount := Low( FCommands ) to High( FCommands ) do
    if FCommands[ iCount ] = aCommand then
      Exit( IOKeyCodeToString( iCount ) );
  Exit( 'ERROR' );
end;

function TLuaConfig.GetKeyCode ( aCommand : Byte ) : TIOKeyCode;
var iCount : Word;
begin
  for iCount := Low( FCommands ) to High( FCommands ) do
    if FCommands[ iCount ] = aCommand then
      Exit( iCount );
  Exit( 0 );
end;

function TLuaConfig.RunKey ( const aKeyID : AnsiString ) : Variant;
begin
  Exit( Entries[FKeyTabName+'.'+aKeyID ] );
end;

function TLuaConfig.RunKey ( aKeyCode : TIOKeyCode ) : Variant;
begin
  Exit( Entries[FKeyTabName+'.'+IOKeyCodeToString(aKeyCode) ] );
end;

procedure TLuaConfig.Load( const aFileName : Ansistring );
begin
  if luaL_dofile(FState, PChar(aFileName)) <> 0 then
    raise ELuaException.Create(lua_tostring(FState,-1));
end;

procedure TLuaConfig.LoadMain(const aFileName: Ansistring);
begin
  FConfigPath := ExtractFilePath( aFileName );
  lua_pushstring( FState, PChar( FConfigPath ) );
  lua_setglobal( FState, '__rootpath' );
  lua_register( FState, 'dofile', @lua_config_dofile );
  Load( aFileName );
end;

procedure TLuaConfig.EntryFeed(const Table: AnsiString;
  const Callback: TEntryCallback);
begin
  if not Resolve( Table ) then raise ELuaException.Create('EntryFeed('+Table+') failed!');
  if not lua_istable( FState, -1 ) then raise ELuaException.Create('EntryFeed('+Table+') target not a table!');

  lua_pushnil( FState );  // first key */
  while (lua_next( FState, -2 ) <> 0) do
  begin
    // uses 'key' (at index -2) and 'value' (at index -1) */
    Callback( vlua_tovariant( FState, -2 ), vlua_tovariant( FState, -1 ) );
    lua_pop( FState, 1 );
  end;
  lua_pop( FState, 1 );
end;

procedure TLuaConfig.RecEntryFeed(const Table: AnsiString;
  const Callback: TEntryCallback);
  procedure Iterate( const KeyStart : AnsiString );
  begin
    lua_pushnil( FState );  // first key */
    while (lua_next( FState, -2 ) <> 0) do
    begin
      // uses 'key' (at index -2) and 'value' (at index -1) */
      if lua_istable( FState, -1 )
        then Iterate( KeyStart + vlua_tovariant( FState, -2 ) + '.' )
        else Callback( KeyStart + vlua_tovariant( FState, -2 ), vlua_tovariant( FState, -1 ) );
      lua_pop( FState, 1 );
    end;
  end;
begin
  if not Resolve( Table ) then raise ELuaException.Create('EntryFeed('+Table+') failed!');
  if not lua_istable( FState, -1 ) then raise ELuaException.Create('EntryFeed('+Table+') target not a table!');

  Iterate('');
  lua_pop( FState, 1 );
end;

procedure TLuaConfig.SetConstant(const ID: AnsiString; const Value: Variant);
begin
  vlua_pushvariant( FState, Value );
  lua_setglobal( FState, ID );
end;

function TLuaConfig.Call ( const Path : array of const; const Args : array of const ) : Variant;
begin
  if not vlua_getpath( FState, Path ) then raise ELuaException.Create('Call('+DebugToString( Path )+') not found!');
  try
    if not lua_isfunction( FState, -1 ) then raise ELuaException.Create('Call('+DebugToString( Path )+') not a function!');
    vlua_pusharray( FState, Args );
    if lua_pcall( FState, High( Args ) + 1, 1, 0 ) <> 0 then  raise ELuaException.Create( 'Call('+DebugToString( Path )+') Lua error : '+lua_tostring( FState, -1) );
    Call := vlua_tovariant( FState, -1, False );
  finally
    lua_pop( FState, 1 );
  end;
end;

function TLuaConfig.Configure ( const ID : AnsiString; aDefault : Variant ) : Variant;
begin
  if HasValue( ID )
    then Exit( GetValue( ID ) )
    else Exit( aDefault );
end;

function TLuaConfig.isPaused: Boolean;
begin
  Exit( FThread <> nil );
end;

function TLuaConfig.Resume: Variant;
var Error : AnsiString;
    Res   : Integer;
begin
  if FThread = nil then Exit( false );
  vlua_pushvariant( FThread, FResult );
  Res := lua_resume( FThread, 1 );
  if (Res <> 0) and (Res <> LUA_YIELD_) then
  begin
    Error := lua_tostring( FThread, -1 );
    lua_pop( FThread, 1 );
    lua_pop( FState, 1 );
    FThread := nil;
    raise Exception.Create( Error );
  end;
  FResult := vlua_tovariant( FThread, -1 );
  Resume := FResult;
  lua_pop( FThread, 1 );
  if Res <> LUA_YIELD_ then
  begin
    lua_pop( FState, 1 );
    FThread := nil;
  end;
end;

procedure TLuaConfig.ResetCommands;
begin
  FillByte(FCommands,IOKeyCodeMax+1,0);
end;

destructor TLuaConfig.Destroy;
begin
  lua_close( FState );
end;

procedure TLuaConfig.CommandCallback ( key, value : Variant ) ;
var iKey     : TIOKeyCode;
    iCommand : Byte;
begin
  if VarIsOrdinal(value)
    then iCommand := value
    else iCommand := COMMAND_INVALID;
  iKey := StringToIOKeyCode(key);
  if iKey = 0 then // TODO : RAISE ERROR
    Log('Unknown keycode - '+ AnsiString( key ) )
  else
    FCommands[iKey] := iCommand
end;

function TLuaConfig.GetValue(const Key: AnsiString): Variant;
begin
  if FThread <> nil then
  begin
    // Signal error?
    lua_pop( FState, 1 );
    FThread := nil;
  end;
  if not Resolve( Key ) then raise ELuaException.Create('GetValue('+Key+') failed!');
  if lua_isfunction( FState, -1 ) then
  begin
    FThread := lua_newthread( FState );
    lua_insert(FState, -2);
    lua_xmove(FState, FThread, 1);
    GetValue := Resume;
    Exit;
  end;
  GetValue := vlua_tovariant( FState, -1 );
  lua_pop( FState, 1 );
end;

function TLuaConfig.HasValue(const Key: AnsiString): Boolean;
begin
  if Resolve( Key ) then
  begin
    HasValue := not lua_isnil( FState, -1 );
    lua_pop( FState, 1 );
  end
  else
    Exit( False );
end;

function TLuaConfig.Resolve(const Key: AnsiString): Boolean;
var Piece : AnsiString;
    Count : DWord;
begin
  Count := 1;
  repeat
    Piece := ExtractDelimited( Count, Key, ['.'] );
    if Piece = '' then break;
    if Count = 1 then
      lua_getglobal( FState, PChar(Piece) )
    else
      if lua_istable( FState, -1 ) then
      begin
        lua_pushstring( FState, PChar(Piece) );
        lua_gettable( FState, -2);
        lua_insert( FState, -2);
        lua_pop( FState, 1);
      end
      else
      begin
        lua_pop( FState, 1 );
        Exit(False);
      end;
    Inc(Count);
  until false;
  if Count = 1 then Exit(False);
  Exit( True );
end;

function TLuaConfig.GetCommand ( Key : TIOKeyCode ) : Byte;
begin
  Exit( FCommands[ Key ] );
end;

procedure TLuaConfig.SetCommand ( Key : TIOKeyCode; Value : Byte ) ;
begin
  FCommands[ Key ] := Value
end;

end.

