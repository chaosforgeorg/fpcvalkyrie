unit vluaconfig;
{$MODE OBJFPC}
interface

uses classes, vnode, vluastack, vioevent, viotypes, vbindings;

type TEntryCallback = procedure ( key, value : Variant ) of object;


type

{ TLuaConfig }

TLuaConfig = class(TVObject)
    constructor Create( const aFileName : Ansistring = ''; aState : PLua_State = nil );
    procedure LoadKeybindings( aContext : TBindingContext; const aTableName : AnsiString = '' );
    function RunKey( const aKeyID : AnsiString ) : Variant;
    function RunKey( aKeyCode : TIOKeyCode ) : Variant;
    procedure Load( const aFileName : Ansistring );
    procedure Load( aStream : TStream; aSize : DWord; aStreamName : AnsiString = 'config_stream' );
    procedure LoadMain( const aFileName : Ansistring );
    function TableExists( const Table : AnsiString ) : Boolean;
    procedure EntryFeed( const Table : AnsiString; const Callback : TEntryCallback );
    procedure RecEntryFeed( const Table : AnsiString; const Callback : TEntryCallback );
    procedure SetConstant( const ID : AnsiString; const Value : Variant );
    function Call(const Path: array of const; const Args: array of const): Variant;
    function Configure( const ID : AnsiString; aDefault : Variant ) : Variant;
    destructor Destroy; override;
  protected
    function GetValue( const Key : AnsiString ) : Variant;
    function HasValue( const Key : AnsiString ) : Boolean;
    function Resolve( const Key : AnsiString ) : Boolean;
  protected
    FState      : PLua_State;
    FStack      : TLuaStack;
    FKeyTabName : AnsiString;
    FConfigPath : AnsiString;
  public
    property ConfigPath : AnsiString read FConfigPath write FConfigPath;
    property Raw : PLua_State read FState;
    property Stack : TLuaStack read FStack;
  end;

implementation

uses sysutils, strutils, variants, vdebug, vlualibrary, vluaext, vluatype, vutil;

type TLuaBindingLoader = class
  constructor Create( aContext : TBindingContext );
  procedure Callback( aKey, aValue : Variant );
private
  FContext : TBindingContext;
end;

constructor TLuaBindingLoader.Create( aContext : TBindingContext );
begin
  FContext := aContext;
end;

procedure TLuaBindingLoader.Callback( aKey, aValue : Variant );
var iKey : TIOKeyCode;
begin
  iKey := StringToIOKeyCode(aKey);
  if iKey = 0 then
    Log(LOGWARN, 'Unknown keycode - '+AnsiString(aKey))
  else if VarIsOrdinal(aValue) then
    FContext.BindKey(iKey, Integer(aValue))
  else
    FContext.BindKey(iKey, BINDING_FORWARD_LUA);
end;

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
  FStack.Init( FState );
end;

procedure TLuaConfig.LoadKeybindings( aContext : TBindingContext; const aTableName : AnsiString );
var iLoader : TLuaBindingLoader;
begin
  if aTableName <> '' then FKeyTabName := aTableName;
  iLoader := TLuaBindingLoader.Create(aContext);
  try
    EntryFeed(FKeyTabName, @iLoader.Callback);
  finally
    iLoader.Free;
  end;
end;

function TLuaConfig.RunKey ( const aKeyID : AnsiString ) : Variant;
begin
  Exit( GetValue( FKeyTabName+'.'+aKeyID ) );
end;

function TLuaConfig.RunKey ( aKeyCode : TIOKeyCode ) : Variant;
begin
  Exit( GetValue( FKeyTabName+'.'+IOKeyCodeToString(aKeyCode) ) );
end;

procedure TLuaConfig.Load( const aFileName : Ansistring );
begin
  if luaL_dofile(FState, PChar(aFileName)) <> 0 then
    raise ELuaException.Create(lua_tostring(FState,-1));
end;

procedure TLuaConfig.Load( aStream : TStream; aSize : DWord; aStreamName : AnsiString = 'config_stream' );
begin
  if vlua_dostream(FState, aStream, aSize, aStreamName ) <> 0 then
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

function TLuaConfig.TableExists( const Table : AnsiString ) : Boolean;
begin
  Result := True;
  if not Resolve( Table ) then Exit( False );
  if not lua_istable( FState, -1 ) then
    Result := False;
  lua_pop( FState, 1 );
end;

procedure TLuaConfig.EntryFeed(const Table: AnsiString;
  const Callback: TEntryCallback);
begin
  if not Resolve( Table ) then raise ELuaException.Create('EntryFeed('+Table+') failed!');
  if not lua_istable( FState, -1 ) then
  raise ELuaException.Create('EntryFeed('+Table+') target not a table!');

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

destructor TLuaConfig.Destroy;
begin
  lua_close( FState );
end;

function TLuaConfig.GetValue(const Key: AnsiString): Variant;
var iError : Ansistring;
begin
  if not Resolve( Key ) then raise ELuaException.Create('GetValue('+Key+') failed!');
  if lua_isfunction( FState, -1 ) then
  begin
    if lua_pcall( FState, 0, 0, 0 ) <> 0 then
    begin
      iError := lua_tostring( FState, -1 );
      lua_pop( FState, 1 );
      raise ELuaException.Create('GetValue('+Key+') - '+iError+'!');
    end;
    GetValue := 0;
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

end.
