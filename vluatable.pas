{$INCLUDE valkyrie.inc}
unit vluatable;
interface
uses sysutils, classes, variants, vlualibrary,
     vluavalue, vrltools, vutil, vvector, vnode;

const
  LuaKeyField  = -2;
  LuaValueield = -1;


type
TLuaValue = vluavalue.TLuaValue;
TLuaTable = class;

TLuaVariantPair = record
  Key   : Variant;
  Value : Variant;
end;

TLuaIndexVariant = record
  Index : Integer;
  Value : Variant;
end;

TLuaValuePair = record
  Key   : TLuaValue;
  Value : TLuaValue;
end;

TLuaIndexValue = record
  Index : Integer;
  Value : TLuaValue;
end;

generic TGLuaEnumerator<T> = object
private
var
  FState   : PLua_State;
  FCurrent : T;
  FIndex   : Integer;
public
  constructor Create( aState : PLua_State; aIndex : Integer );
public
  property Current : T read FCurrent;
end;

TLuaKeyValueEnumerator = object( specialize TGLuaEnumerator<TLuaVariantPair> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaKeyValueEnumerator;
end;

TLuaIndexValueEnumerator = object( specialize TGLuaEnumerator<TLuaIndexVariant> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaIndexValueEnumerator;
end;

TLuaValuesEnumerator = object( specialize TGLuaEnumerator<Variant> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaValuesEnumerator;
end;

TLuaKeysEnumerator = object( specialize TGLuaEnumerator<Variant> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaKeysEnumerator;
end;

TLuaValueKeyValueEnumerator = object( specialize TGLuaEnumerator<TLuaValuePair> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaValueKeyValueEnumerator;
end;

TLuaValueIndexValueEnumerator = object( specialize TGLuaEnumerator<TLuaIndexValue> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaValueIndexValueEnumerator;
end;

TLuaValueValuesEnumerator = object( specialize TGLuaEnumerator<TLuaValue> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaValueValuesEnumerator;
end;

TLuaValueKeysEnumerator = object( specialize TGLuaEnumerator<TLuaValue> )
public
  function MoveNext : Boolean;
  function GetEnumerator : TLuaValueKeysEnumerator;
end;

{ TLuaTablesEnumerator }

TLuaTablesEnumerator = object
private
  FState   : PLua_State;
  FStack   : Integer;
  FIndex   : Integer;
  FCurrent : TLuaTable;
public
  constructor Create( aState : PLua_State; aIndex : Integer );
  constructor Create( aState : PLua_State; const aPath : array of const; aIndex : Integer = 0 );
  constructor Create( aState : PLua_State; const aPath : AnsiString; aIndex : Integer = 0 );
  destructor Destroy;
  function MoveNext : Boolean;
  function GetEnumerator : TLuaTablesEnumerator;
public
  property Current : TLuaTable read FCurrent;
end;

{ TLuaITablesEnumerator }

TLuaITablesEnumerator = object
private
  FState   : PLua_State;
  FStack   : Integer;
  FIndex   : Integer;
  FSize    : Integer;
  FOrdinal : Integer;
  FCurrent : TLuaTable;
public
  constructor Create( aState : PLua_State; aIndex : Integer );
  constructor Create( aState : PLua_State; const aPath : array of const; aIndex : Integer = 0 );
  constructor Create( aState : PLua_State; const aPath : AnsiString; aIndex : Integer = 0 );
  destructor Destroy;
  function MoveNext : Boolean;
  function GetEnumerator : TLuaITablesEnumerator;
public
  property Current : TLuaTable read FCurrent;
end;

{ TLuaTable }

TLuaTable = class(TVObject)
  constructor Create( L : PLua_State );
  constructor Create( L : PLua_State; const aPath : array of const; aIndex : Integer = 0 );
  constructor Create( L : PLua_State; const aPath : AnsiString; aIndex : Integer = 0 );

  function VariantPairs : TLuaKeyValueEnumerator;
  function IndexVariants : TLuaIndexValueEnumerator;
  function VariantKeys : TLuaKeysEnumerator;
  function VariantValues : TLuaValuesEnumerator;
  function Pairs : TLuaValueKeyValueEnumerator;
  function IPairs : TLuaValueIndexValueEnumerator;
  function Keys : TLuaValueKeysEnumerator;
  function Values : TLuaValueValuesEnumerator;
  function GetEnumerator : TLuaValueKeyValueEnumerator;

  function GetSize : DWord;
  function GetTable( const aPath : Ansistring ) : TLuaTable;
  function GetTable( const aPath : array of const ) : TLuaTable;
  function GetTableSize( const Path : AnsiString ) : DWord;
  function GetTableSize( const Path : array of Const ) : DWord;

  function Tables( const aPath : Ansistring ) : TLuaTablesEnumerator;
  function Tables( const aPath : array of const ) : TLuaTablesEnumerator;
  function ITables( const aPath : Ansistring ) : TLuaITablesEnumerator;
  function ITables( const aPath : array of const ) : TLuaITablesEnumerator;

  function GetValue( const aKey : Variant ) : Variant;
  function GetValue( const aPath : Ansistring ) : Variant;
  function GetValue( const aPath : array of const ) : Variant;
  procedure SetValue( const aKey : Variant; const aValue : Variant );
  procedure SetValue( const aPath : Ansistring; const aValue : Variant );
  procedure SetValue( const aPath : array of const; const aValue : Variant );

  function GetField( const aKey : AnsiString ) : Variant;
  procedure SetField( const aKey : AnsiString; const aValue : Variant );

  function GetInteger( const aKey : AnsiString ) : LongInt; overload;
  function GetFloat( const aKey : AnsiString ) : Double; overload;
  function GetString( const aKey : AnsiString ) : AnsiString; overload;
  function GetChar( const aKey : AnsiString ) : Char; overload;
  function GetBoolean( const aKey : AnsiString ) : boolean; overload;
  function GetFlags( const aKey : AnsiString ) : TFlags; overload;

  function GetInteger( const aKey : AnsiString; aDefault : LongInt ) : LongInt; overload;
  function GetFloat( const aKey : AnsiString; aDefault : Double ) : Double; overload;
  function GetString( const aKey : AnsiString; const aDefault : AnsiString ) : AnsiString; overload;
  function GetChar( const aKey : AnsiString; aDefault : Char ) : Char; overload;
  function GetBoolean( const aKey : AnsiString; aDefault : Boolean ) : boolean; overload;
  function GetFlags( const aKey : AnsiString; const aDefault : TFlags ) : TFlags; overload;

  function GetVec2f( const aKey : AnsiString ) : TVec2f; overload;
  function GetVec3f( const aKey : AnsiString ) : TVec3f; overload;
  function GetVec4f( const aKey : AnsiString ) : TVec4f; overload;
  function GetVec2i( const aKey : AnsiString ) : TVec2i; overload;
  function GetVec3i( const aKey : AnsiString ) : TVec3i; overload;
  function GetVec4i( const aKey : AnsiString ) : TVec4i; overload;
  function GetVec2b( const aKey : AnsiString ) : TVec2b; overload;
  function GetVec3b( const aKey : AnsiString ) : TVec3b; overload;
  function GetVec4b( const aKey : AnsiString ) : TVec4b; overload;

  function GetObject( const aKey : AnsiString ) : TObject;
  function GetObjectOrNil( const aKey : AnsiString ) : TObject;
  function GetStringArray( const aKey : AnsiString ) : TAnsiStringArray;
  function GetCoord( const aKey : AnsiString ) : TCoord2D;
  function GetArea( const aKey : AnsiString ) : TArea;
  function GetPoint( const aKey : AnsiString ) : TPoint;
  function GetRect( const aKey : AnsiString ) : TRectangle;
  function GetFunction( const aKey : AnsiString ) : lua_Cfunction;

  function IsNil( const aKey : AnsiString ) : Boolean;
  function IsNumber( const aKey : AnsiString ) : Boolean;
  function IsBoolean( const aKey : AnsiString ) : Boolean;
  function IsString( const aKey : AnsiString ) : Boolean;
  function IsTable( const aKey : AnsiString ) : Boolean;
  function IsObject( const aKey : AnsiString ) : Boolean;
  function IsCoord( const aKey : AnsiString ) : Boolean;
  function IsArea( const aKey : AnsiString ) : Boolean;
  function IsPoint( const aKey : AnsiString ) : Boolean;
  function IsRect( const aKey : AnsiString ) : Boolean;
  function IsFunction( const aKey : AnsiString ) : Boolean;

  procedure SetInteger( const aKey : AnsiString; aValue : LongInt );
  procedure SetFloat( const aKey : AnsiString; aValue : Double );
  procedure SetString( const aKey : AnsiString; const aValue : AnsiString );
  procedure SetChar( const aKey : AnsiString; aValue : Char );
  procedure SetBoolean( const aKey : AnsiString; aValue : Boolean );
  procedure SetFlagsFlat( const aKey : AnsiString; const aValue : TFlags );
  procedure SetFlagsSet( const aKey : AnsiString; const aValue : TFlags );
  procedure SetCoord( const aKey : AnsiString; const aValue : TCoord2D );
  procedure SetArea( const aKey : AnsiString; const aValue : TArea );
  procedure SetPoint( const aKey : AnsiString; const aValue : TPoint );
  procedure SetRect( const aKey : AnsiString; const aValue : TRectangle );
  procedure SetObject( const aKey : AnsiString; aObject : TObject );
  procedure SetFunction( const aKey : AnsiString; func : lua_Cfunction );

  // Call a function
  function Call( const aName : AnsiString; const Args : array of Const ) : Variant;
  // Call a function
  function Call( const aName : AnsiString; const Args : array of Const; aDefault : Variant ) : Variant;
  // Call a function in protected mode -- exceptions will be caught, logged,
  // reported to OnError. False will be returned on Error;
  function ProtectedCall( const aName : AnsiString; const Args : array of Const ) : Variant;
  // Call a function in protected mode -- exceptions will be caught, logged,
  // reported to OnError. aDefault will be returned on Error;
  function ProtectedCall( const aName : AnsiString; const Args : array of Const; aDefault : Variant ) : Variant;

  procedure Reset; inline;
  destructor Destroy; override;
private
  procedure Push; inline;
  procedure RunGetField( const aKey : AnsiString ); inline;
  function TryGetField( const aKey : AnsiString ) : Boolean; inline;
  procedure RunGetField( const aKey : AnsiString; ReqType : Byte ); inline;
  function TryGetField( const aKey : AnsiString; ReqType : Byte ) : Boolean; inline;
  function IsField( const aKey : AnsiString; ReqType : Byte ) : Boolean; inline;
private
  FClear : Integer;
  FState : PLua_State;
  FRef   : Integer;
public
end;


implementation
uses strutils, vluasystem, vdebug, vluaext, vluatools, vluatype;

{ TLuaITablesEnumerator }

constructor TLuaITablesEnumerator.Create(aState: PLua_State; aIndex: Integer);
begin
  FState   := aState;
  FIndex   := lua_absindex( FState, aIndex );
  FStack   := lua_gettop( FState );
  FCurrent := nil;
  FSize    := lua_objlen( FState, aIndex );
  FOrdinal := -1;
end;

constructor TLuaITablesEnumerator.Create(aState: PLua_State; const aPath: array of const; aIndex : Integer);
begin
  FState := aState;
  FStack := lua_gettop( FState );
  if not vlua_getpath( FState, aPath, aIndex ) then raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is nil!' );
  if not lua_istable( FState, -1 ) then
  begin
    lua_pop( FState, 1 );
    raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is not a table!' );
  end;
  FIndex   := lua_absindex( FState, -1 );
  FSize    := lua_objlen( FState, FIndex );
  FOrdinal := -1;
  FCurrent := nil;
end;

constructor TLuaITablesEnumerator.Create(aState: PLua_State; const aPath: AnsiString; aIndex : Integer);
begin
  FState := aState;
  FStack := lua_gettop( FState );
  if not vlua_getpath( FState, aPath, aIndex ) then raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is nil!' );
  if not lua_istable( FState, -1 ) then
  begin
    lua_pop( FState, 1 );
    raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is not a table!' );
  end;
  FIndex   := lua_absindex( FState, -1 );
  FSize    := lua_objlen( FState, FIndex );
  FOrdinal := -1;
  FCurrent := nil;
end;

destructor TLuaITablesEnumerator.Destroy;
begin
  FreeAndNil( FCurrent );
  lua_settop( FState, FStack );
end;

function TLuaITablesEnumerator.MoveNext: Boolean;
begin
  if FCurrent <> nil then
  begin
    FreeAndNil( FCurrent );
    lua_pop( FState, 1 );
  end;
  Inc( FOrdinal );
  MoveNext := FOrdinal < FSize;
  if MoveNext then
  begin
    lua_rawgeti( FState, FIndex, FOrdinal+1 );
    FCurrent := TLuaTable.Create( FState );
  end;
end;

function TLuaITablesEnumerator.GetEnumerator: TLuaITablesEnumerator;
begin
  Exit( Self );
end;

{ TLuaTablesEnumerator }

constructor TLuaTablesEnumerator.Create(aState: PLua_State; aIndex: Integer);
begin
  FState   := aState;
  FIndex   := lua_absindex( FState, aIndex );
  FStack   := lua_gettop( FState );
  FCurrent := nil;
  lua_pushnil( FState );
end;

constructor TLuaTablesEnumerator.Create(aState: PLua_State; const aPath: array of const; aIndex : Integer = 0 );
begin
  FState := aState;
  FStack := lua_gettop( FState );
  if not vlua_getpath( FState, aPath, aIndex ) then raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is nil!' );
  if not lua_istable( FState, -1 ) then
  begin
    lua_pop( FState, 1 );
    raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is not a table!' );
  end;
  FIndex   := lua_absindex( FState, -1 );
  FCurrent := nil;
  lua_pushnil( FState );
end;

constructor TLuaTablesEnumerator.Create(aState: PLua_State; const aPath: AnsiString; aIndex : Integer = 0 );
begin
  FState := aState;
  FStack := lua_gettop( FState );
  if not vlua_getpath( FState, aPath, aIndex ) then raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is nil!' );
  if not lua_istable( FState, -1 ) then
  begin
    lua_pop( FState, 1 );
    raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTablesEnumerator is not a table!' );
  end;
  FIndex   := lua_absindex( FState, -1 );
  FCurrent := nil;
  lua_pushnil( FState );
end;

destructor TLuaTablesEnumerator.Destroy;
begin
  FreeAndNil( FCurrent );
  lua_settop( FState, FStack );
end;

function TLuaTablesEnumerator.MoveNext: Boolean;
begin
  if FCurrent <> nil then
  begin
    FreeAndNil( FCurrent );
    lua_pop( FState, 1 );
  end;
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
  begin
    FCurrent := TLuaTable.Create( FState );
  end;
end;

function TLuaTablesEnumerator.GetEnumerator: TLuaTablesEnumerator;
begin
  Exit( Self );
end;

{ TLuaKeysEnumerator }

function TLuaKeysEnumerator.MoveNext : Boolean;
begin
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
  begin
    FCurrent := vlua_tovariant( FState, -2 );
    lua_pop( FState, 1 );
  end;
end;

function TLuaKeysEnumerator.GetEnumerator : TLuaKeysEnumerator;
begin
  Exit( Self );
end;

{ TLuaValuesEnumerator }

function TLuaValuesEnumerator.MoveNext : Boolean;
begin
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
  begin
    FCurrent := vlua_tovariant( FState, -1 );
    lua_pop( FState, 1 );
  end;
end;

function TLuaValuesEnumerator.GetEnumerator : TLuaValuesEnumerator;
begin
  Exit( Self );
end;

{ TGLuaEnumerator }

constructor TGLuaEnumerator.Create ( aState : PLua_State; aIndex : Integer ) ;
begin
  FState := aState;
  FIndex := lua_absindex( FState, aIndex );
  lua_pushnil( FState );
end;


{ TLuaIndexValueEnumerator }

function TLuaIndexValueEnumerator.MoveNext : Boolean;
begin
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
  begin
    FCurrent.Index := lua_tointeger( FState, -2 );
    FCurrent.Value := vlua_tovariant( FState, -1 );
    lua_pop( FState, 1 );
  end;
end;

function TLuaIndexValueEnumerator.GetEnumerator : TLuaIndexValueEnumerator;
begin
  Exit( Self );
end;

{ TLuaKeyValueEnumerator }

function TLuaKeyValueEnumerator.MoveNext : Boolean;
begin
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
  begin
    FCurrent.Key   := vlua_tovariant( FState, -2 );
    FCurrent.Value := vlua_tovariant( FState, -1 );
    lua_pop( FState, 1 );
  end;
end;

function TLuaKeyValueEnumerator.GetEnumerator : TLuaKeyValueEnumerator;
begin
  Exit( Self );
end;

{ TLuaValueKeyValueEnumerator }

function TLuaValueKeyValueEnumerator.MoveNext : Boolean;
begin
  if not lua_isnil( FState, -1 ) then lua_pop( FState, 1 );
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
  begin
    FCurrent.Key.Create( FState, -2 );
    FCurrent.Value.Create( FState, -1 );
  end;
end;

function TLuaValueKeyValueEnumerator.GetEnumerator : TLuaValueKeyValueEnumerator;
begin
  Exit( Self );
end;

{ TLuaValueIndexValueEnumerator }

function TLuaValueIndexValueEnumerator.MoveNext : Boolean;
begin
  if not lua_isnil( FState, -1 ) then lua_pop( FState, 1 );
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
  begin
    FCurrent.Index := lua_tointeger( FState, -2 );
    FCurrent.Value.Create( FState, -1 );
  end;
end;

function TLuaValueIndexValueEnumerator.GetEnumerator : TLuaValueIndexValueEnumerator;
begin
  Exit( Self );
end;

{ TLuaValueKeysEnumerator }

function TLuaValueKeysEnumerator.MoveNext : Boolean;
begin
  if not lua_isnil( FState, -1 ) then lua_pop( FState, 1 );
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
    FCurrent.Create( FState, -2 );
end;

function TLuaValueKeysEnumerator.GetEnumerator : TLuaValueKeysEnumerator;
begin
  Exit( Self );
end;

{ TLuaValueValuesEnumerator }

function TLuaValueValuesEnumerator.MoveNext : Boolean;
begin
  if not lua_isnil( FState, -1 ) then lua_pop( FState, 1 );
  MoveNext := lua_next( FState, FIndex ) <> 0;
  if MoveNext then
    FCurrent.Create( FState, -1 );
end;

function TLuaValueValuesEnumerator.GetEnumerator : TLuaValueValuesEnumerator;
begin
  Exit( Self );
end;

{ TLuaTable }

constructor TLuaTable.Create( L : PLua_State ) ;
begin
  inherited Create;
  FClear := lua_gettop( L );
  FState := L;
  if not lua_istable( L, -1 ) then raise ELuaException.Create( 'Object passed to TLuaTable is not a table!' );
  FRef   := luaL_ref( FState, LUA_REGISTRYINDEX );
end;

constructor TLuaTable.Create ( L : PLua_State; const aPath : array of const; aIndex : Integer = 0 );
begin
  inherited Create;
  FClear := lua_gettop( L );
  FState := L;
  if not vlua_getpath( L, aPath, aIndex ) then raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTable is nil!' );
  if not lua_istable( L, -1 ) then
  begin
    lua_pop( L, 1 );
    raise ELuaException.Create( 'Path ['+DebugToString(aPath)+'] passed to TLuaTable is not a table!' );
  end;
  FRef   := luaL_ref( FState, LUA_REGISTRYINDEX );
end;

constructor TLuaTable.Create ( L : PLua_State; const aPath : AnsiString; aIndex : Integer = 0 );
begin
  inherited Create;
  FClear := lua_gettop( L );
  FState := L;
  if not vlua_getpath( L, aPath, aIndex ) then raise ELuaException.Create( 'Path ['+aPath+'] passed to TLuaTable is nil!' );
  if not lua_istable( L, -1 ) then
  begin
    lua_pop( L, 1 );
    raise ELuaException.Create( 'Path ['+aPath+'] passed to TLuaTable is not a table!' );
  end;
  FRef   := luaL_ref( FState, LUA_REGISTRYINDEX );
end;

function TLuaTable.VariantPairs : TLuaKeyValueEnumerator;
begin
  Push;
  VariantPairs.Create( FState, -1 );
end;

function TLuaTable.IndexVariants : TLuaIndexValueEnumerator;
begin
  Push;
  IndexVariants.Create( FState, -1 );
end;

function TLuaTable.VariantKeys : TLuaKeysEnumerator;
begin
  Push;
  VariantKeys.Create( FState, -1 );
end;

function TLuaTable.VariantValues : TLuaValuesEnumerator;
begin
  Push;
  VariantValues.Create( FState, -1 );
end;

function TLuaTable.Pairs : TLuaValueKeyValueEnumerator;
begin
  Push;
  Pairs.Create( FState, -1 );
end;

function TLuaTable.IPairs : TLuaValueIndexValueEnumerator;
begin
  Push;
  IPairs.Create( FState, -1 );
end;

function TLuaTable.Keys : TLuaValueKeysEnumerator;
begin
  Push;
  Keys.Create( FState, -1 );
end;

function TLuaTable.Values : TLuaValueValuesEnumerator;
begin
  Push;
  Values.Create( FState, -1 );
end;

function TLuaTable.GetEnumerator : TLuaValueKeyValueEnumerator;
begin
  Push;
  GetEnumerator.Create( FState, -1 );
end;

function TLuaTable.GetSize: DWord;
begin
  Push;
  GetSize := lua_objlen( FState, -1 );
  Reset;
end;

function TLuaTable.GetTable(const aPath: Ansistring): TLuaTable;
begin
  Push;
  GetTable := TLuaTable.Create( FState, aPath, -1 );
  Reset;
end;

function TLuaTable.GetTable(const aPath: array of const): TLuaTable;
begin
  Push;
  GetTable := TLuaTable.Create( FState, aPath, -1 );
  Reset;
end;

function TLuaTable.GetTableSize(const Path: AnsiString): DWord;
begin
  Push;
  GetTableSize := 0;
  if vlua_getpath( FState, Path, -1 ) then
  begin
    if lua_istable( FState, -1 ) then
      GetTableSize := lua_objlen( FState, -1 );
    lua_pop( FState, 1 );
  end;
  Reset;
end;

function TLuaTable.GetTableSize(const Path: array of const): DWord;
begin
  Push;
  GetTableSize := 0;
  if vlua_getpath( FState, Path, -1 ) then
  begin
    if lua_istable( FState, -1 ) then
      GetTableSize := lua_objlen( FState, -1 );
    lua_pop( FState, 1 );
  end;
  Reset;
end;

function TLuaTable.Tables(const aPath: Ansistring): TLuaTablesEnumerator;
begin
  Push;
  Tables.Create( FState, aPath, -1 );
  Tables.FStack -= 1;
end;

function TLuaTable.Tables(const aPath: array of const): TLuaTablesEnumerator;
begin
  Push;
  Tables.Create( FState, aPath, -1 );
  Tables.FStack -= 1;
end;

function TLuaTable.ITables(const aPath: Ansistring): TLuaITablesEnumerator;
begin
  Push;
  ITables.Create( FState, aPath, -1 );
  ITables.FStack -= 1;
end;

function TLuaTable.ITables(const aPath: array of const): TLuaITablesEnumerator;
begin
  Push;
  ITables.Create( FState, aPath, -1 );
  ITables.FStack -= 1;
end;

function TLuaTable.GetValue ( const aKey : Variant ) : Variant;
begin
  Push;
  vlua_pushvariant( FState, aKey );
  lua_rawget( FState, -2 );
  GetValue := vlua_tovariant( FState, -1 );
  Reset;
end;

function TLuaTable.GetValue ( const aPath : Ansistring ) : Variant;
begin
  Push;
  if not vlua_getpath( FState, aPath, -1 ) then raise ELuaException.Create('GetValue('+DebugToString( aPath )+') failed!');
  GetValue := vlua_tovariant( FState, -1 );
  lua_pop( FState, 2 );
end;

function TLuaTable.GetValue ( const aPath : array of const ) : Variant;
begin
  Push;
  if not vlua_getpath( FState, aPath, -1 ) then raise ELuaException.Create('GetValue('+DebugToString( aPath )+') failed!');
  GetValue := vlua_tovariant( FState, -1 );
  lua_pop( FState, 2 );
end;

procedure TLuaTable.SetValue ( const aKey : Variant; const aValue : Variant ) ;
begin
  Push;
  vlua_pushvariant( FState, aKey );
  vlua_pushvariant( FState, aValue );
  lua_rawset( FState, -3 );
  Reset;
end;

procedure TLuaTable.SetValue ( const aPath : Ansistring; const aValue : Variant ) ;
var RP   : Word;
    Key  : AnsiString;
begin
  Push;
  RP := RPos( '.', aPath );
  if RP < 1 then
  begin
    vlua_pushvariant( FState, aValue );
    lua_setfield( FState, -2, PChar(aPath) );
  end
  else
  begin
    if not vlua_getpath( FState, LeftStr( aPath, RP-1 ) ) then raise ELuaException.Create('SetValue('+aPath+') failed!');
    vlua_pushvariant( FState, aValue );
    Key := Copy( aPath, RP+1, Length( aPath ) - RP );
    lua_setfield( FState, -2, PChar(Key) );
  end;
  Reset;
end;

procedure TLuaTable.SetValue ( const aPath : array of const; const aValue : Variant ) ;
begin
  Push;
  if High( aPath ) = 0 then
  begin
    vlua_pushvarrec( FState, @aPath[0] );
    vlua_pushvariant( FState, aValue );
    lua_rawset( FState, -3 );
  end
  else
  begin
    if not vlua_getpath( FState, aPath, High(aPath) - 1 ) then raise ELuaException.Create('SetValue('+DebugToString( aPath )+') failed!');
    vlua_pushvarrec( FState, @aPath[High( aPath )] );
    vlua_pushvariant( FState, aValue );
    lua_rawset( FState, -3 );
  end;
  Reset;
end;

function TLuaTable.GetField ( const aKey : AnsiString ) : Variant;
begin
  Push;
  lua_getfield( FState, -1, PChar(aKey) );
  GetField := vlua_tovariant( FState, -1 );
  Reset;
end;

procedure TLuaTable.SetField ( const aKey : AnsiString; const aValue : Variant ) ;
begin
  Push;
  vlua_pushvariant( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

function TLuaTable.GetInteger ( const aKey : AnsiString ) : LongInt;
begin
  Push;
  RunGetField( aKey );
  Result := lua_tointeger( FState, -1 );
  Reset;
end;

function TLuaTable.GetFloat ( const aKey : AnsiString ) : Double;
begin
  Push;
  RunGetField( aKey );
  Result := lua_tonumber( FState, -1 );
  Reset;
end;

function TLuaTable.GetString ( const aKey : AnsiString ) : AnsiString;
begin
  Push;
  RunGetField( aKey );
  Result := lua_tostring( FState, -1 );
  Reset;
end;

function TLuaTable.GetChar ( const aKey : AnsiString ) : Char;
begin
  Push;
  RunGetField( aKey, LUA_TSTRING );
  Result := vlua_tochar( FState, -1 );
  Reset;
end;

function TLuaTable.GetBoolean ( const aKey : AnsiString ) : boolean;
begin
  Push;
  RunGetField( aKey );
  Result := lua_toboolean( FState, -1 );
  Reset;
end;

function TLuaTable.GetFlags ( const aKey : AnsiString ) : TFlags;
begin
  Push;
  RunGetField( aKey, LUA_TTABLE );
  Result := vlua_toflags( FState, -1 );
  Reset;
end;

function TLuaTable.GetInteger ( const aKey : AnsiString; aDefault : LongInt ) : LongInt;
begin
  Push;
  if TryGetField( aKey, LUA_TNUMBER )
    then Result := lua_tointeger( FState, -1 )
    else Result := aDefault;
  Reset;
end;

function TLuaTable.GetFloat ( const aKey : AnsiString; aDefault : Double ) : Double;
begin
  Push;
  if TryGetField( aKey, LUA_TNUMBER )
    then Result := lua_tonumber( FState, -1 )
    else Result := aDefault;
  Reset;
end;

function TLuaTable.GetString ( const aKey : AnsiString; const aDefault : AnsiString ) : AnsiString;
begin
  Push;
  if TryGetField( aKey, LUA_TSTRING )
    then Result := lua_tostring( FState, -1 )
    else Result := aDefault;
  Reset;
end;

function TLuaTable.GetChar ( const aKey : AnsiString; aDefault : Char ) : Char;
begin
  Push;
  if TryGetField( aKey, LUA_TSTRING ) and vlua_ischar( FState, -1 )
    then Result := vlua_tochar( FState, -1 )
    else Result := aDefault;
  Reset;
end;

function TLuaTable.GetBoolean ( const aKey : AnsiString; aDefault : Boolean ) : boolean;
begin
  Push;
  if TryGetField( aKey, LUA_TBOOLEAN )
    then Result := lua_toboolean( FState, -1 )
    else Result := aDefault;
  Reset;
end;

function TLuaTable.GetFlags ( const aKey : AnsiString; const aDefault : TFlags ) : TFlags;
begin
  Push;
  if TryGetField( aKey, LUA_TTABLE )
    then Result := vlua_toflags( FState, -1 )
    else Result := aDefault;
  Reset;
end;

function TLuaTable.GetVec2f ( const aKey : AnsiString ) : TVec2f;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec2f( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec3f ( const aKey : AnsiString ) : TVec3f;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec3f( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec4f ( const aKey : AnsiString ) : TVec4f;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec4f( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec2i ( const aKey : AnsiString ) : TVec2i;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec2i( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec3i ( const aKey : AnsiString ) : TVec3i;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec3i( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec4i ( const aKey : AnsiString ) : TVec4i;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec4i( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec2b ( const aKey : AnsiString ) : TVec2b;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec2b( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec3b ( const aKey : AnsiString ) : TVec3b;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec3b( FState, -1 );
  Reset;
end;

function TLuaTable.GetVec4b ( const aKey : AnsiString ) : TVec4b;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tovec4b( FState, -1 );
  Reset;
end;

function TLuaTable.GetObject ( const aKey : AnsiString ) : TObject;
begin
  Push;
  RunGetField( aKey, LUA_TTABLE );
  Result := vlua_toobject( FState, -1 );
  if Result = nil then raise ELuaException.Create( 'Object expected at key '+aKey+'!' );
  Reset;
end;

function TLuaTable.GetObjectOrNil ( const aKey : AnsiString ) : TObject;
begin
  Push;
  if not TryGetField( aKey, LUA_TTABLE ) then Exit( nil );
  Result := vlua_toobject( FState, -1 );
  Reset;
end;

function TLuaTable.GetStringArray ( const aKey : AnsiString ) : TAnsiStringArray;
begin
  Push;
  RunGetField( aKey, LUA_TTABLE );
  Result := vlua_tostringarray( FState, -1 );
  Reset;
end;

function TLuaTable.GetCoord ( const aKey : AnsiString ) : TCoord2D;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_tocoord( FState, -1 );
  Reset;
end;

function TLuaTable.GetArea ( const aKey : AnsiString ) : TArea;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_toarea( FState, -1 );
  Reset;
end;

function TLuaTable.GetPoint(const aKey: AnsiString): TPoint;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_topoint( FState, -1 );
  Reset;
end;

function TLuaTable.GetRect(const aKey: AnsiString): TRectangle;
begin
  Push;
  RunGetField( aKey );
  Result := vlua_torect( FState, -1 );
  Reset;
end;

function TLuaTable.GetFunction ( const aKey : AnsiString ) : lua_Cfunction;
begin
  Push;
  RunGetField( aKey );
  Result := lua_tocfunction( FState, -1 );
  Reset;
end;

function TLuaTable.IsNil ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  lua_getfield( FState, -1, PChar( aKey ) );
  Result := lua_isnoneornil( FState, -1 );
  Reset;
end;

function TLuaTable.IsNumber ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  Result := IsField( aKey, LUA_TNUMBER );
  Reset;
end;

function TLuaTable.IsBoolean ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  Result := IsField( aKey, LUA_TBOOLEAN );
  Reset;
end;

function TLuaTable.IsString ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  Result := IsField( aKey, LUA_TSTRING );
  Reset;
end;

function TLuaTable.IsTable ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  Result := IsField( aKey, LUA_TTABLE );
  Reset;
end;

function TLuaTable.IsObject ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  lua_getfield( FState, -1, PChar( aKey ) );
  Result := vlua_isobject( FState, -1 );
  Reset;
end;

function TLuaTable.IsCoord ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  lua_getfield( FState, -1, PChar( aKey ) );
  Result := vlua_iscoord( FState, -1 );
  Reset;
end;

function TLuaTable.IsArea ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  lua_getfield( FState, -1, PChar( aKey ) );
  Result := vlua_isarea( FState, -1 );
  Reset;
end;

function TLuaTable.IsPoint(const aKey: AnsiString): Boolean;
begin
  Push;
  lua_getfield( FState, -1, PChar( aKey ) );
  Result := vlua_ispoint( FState, -1 );
  Reset;
end;

function TLuaTable.IsRect(const aKey: AnsiString): Boolean;
begin
  Push;
  lua_getfield( FState, -1, PChar( aKey ) );
  Result := vlua_isrect( FState, -1 );
  Reset;
end;

function TLuaTable.IsFunction ( const aKey : AnsiString ) : Boolean;
begin
  Push;
  Result := IsField( aKey, LUA_TFUNCTION );
  Reset;
end;

procedure TLuaTable.SetInteger ( const aKey : AnsiString; aValue : LongInt ) ;
begin
  Push;
  lua_pushinteger( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetFloat ( const aKey : AnsiString; aValue : Double ) ;
begin
  Push;
  lua_pushnumber( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetString ( const aKey : AnsiString; const aValue : AnsiString ) ;
begin
  Push;
  lua_pushansistring( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetChar ( const aKey : AnsiString; aValue : Char ) ;
begin
  Push;
  lua_pushansistring( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetBoolean ( const aKey : AnsiString; aValue : Boolean ) ;
begin
  Push;
  lua_pushboolean( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetFlagsFlat ( const aKey : AnsiString; const aValue : TFlags ) ;
begin
  Push;
  vlua_pushflags_array( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetFlagsSet ( const aKey : AnsiString; const aValue : TFlags ) ;
begin
  Push;
  vlua_pushflags_set( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetCoord ( const aKey : AnsiString; const aValue : TCoord2D ) ;
begin
  Push;
  vlua_pushcoord( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetArea ( const aKey : AnsiString; const aValue : TArea ) ;
begin
  Push;
  vlua_pusharea( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetPoint(const aKey: AnsiString; const aValue: TPoint);
begin
  Push;
  vlua_pushpoint( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetRect(const aKey: AnsiString; const aValue: TRectangle);
begin
  Push;
  vlua_pushrect( FState, aValue );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetObject( const aKey : AnsiString; aObject : TObject );
begin
  Push;
  vlua_pushobject( FState, aObject );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

procedure TLuaTable.SetFunction ( const aKey : AnsiString; func : lua_Cfunction ) ;
begin
  Push;
  lua_pushcfunction( FState, func );
  lua_setfield( FState, -2, PChar(aKey) );
  Reset;
end;

function TLuaTable.Call ( const aName : AnsiString;
  const Args : array of const ) : Variant;
begin
  Exit( Call( aName, Args, False ) );
end;

function TLuaTable.Call ( const aName : AnsiString;
  const Args : array of const; aDefault : Variant ) : Variant;
begin
  Push;
  lua_getfield( FState, -1, PChar( aName ) );
  try
    if not lua_isfunction( FState, -1 ) then raise ELuaException.Create('Table function '+aName+' not found!');
    vlua_pusharray( FState, Args );
    if lua_pcall( FState, High( Args ) + 1, 1, 0 ) <> 0 then raise ELuaException.Create( 'Table function '+aName+' - Lua error : '+lua_tostring( FState, -1) );
    Call := vlua_tovariant( FState, -1, aDefault );
  finally
    lua_pop( FState, 1 );
  end;
  Reset;
end;

function TLuaTable.ProtectedCall ( const aName : AnsiString;
  const Args : array of const ) : Variant;
begin
  Exit( ProtectedCall( aName, Args, False ) );
end;

function TLuaTable.ProtectedCall ( const aName : AnsiString;
  const Args : array of const; aDefault : Variant ) : Variant;
begin
  try
    Exit( Call( aName, Args, aDefault ) );
  except on e : Exception do
  begin
    ErrorLogOpen('ERROR','Lua call UNKNOWN_TABLE.'+aName+' caught '+e.ClassName+'!');
    ErrorLogWriteln('Call path     : UNKNOWN_TABLE.'+aName );
    ErrorLogWriteln('Call params   : '+DebugToString( Args ));
    ErrorLogWriteln('Error message : '+e.Message);
    ErrorLogClose;
    ProtectedCall := aDefault;
    LuaSystem.OnError( aName + ' -- ' + e.Message );
  end;
  end;
end;

procedure TLuaTable.Reset;
begin
  lua_settop( FState, FClear );
end;

destructor TLuaTable.Destroy;
begin
  lua_settop( FState, FClear );
  luaL_unref( FState, LUA_REGISTRYINDEX, FRef );
  inherited Destroy;
end;

procedure TLuaTable.Push;
begin
  lua_settop( FState, FClear );
  lua_rawgeti( FState, LUA_REGISTRYINDEX, FRef );
end;

procedure TLuaTable.RunGetField ( const aKey : AnsiString ) ;
begin
  lua_getfield( FState, -1, PChar( aKey ) );
  if lua_isnil( FState, -1 ) then raise ELuaException.Create( 'Field '+aKey+' not found!' );
end;

function TLuaTable.TryGetField ( const aKey : AnsiString ) : Boolean;
begin
  lua_getfield( FState, -1, PChar( aKey ) );
  if lua_isnil( FState, -1 ) then Exit( False );
  Result := True;
end;

procedure TLuaTable.RunGetField ( const aKey : AnsiString; ReqType : Byte ) ;
begin
  lua_getfield( FState, -1, PChar( aKey ) );
  if lua_type( FState, -1 ) <> ReqType then ELuaException.Create( 'Field '+aKey+' not found, or wrong type!' );
end;

function TLuaTable.TryGetField ( const aKey : AnsiString; ReqType : Byte ) : Boolean;
begin
  lua_getfield( FState, -1, PChar( aKey ) );
  if lua_type( FState, -1 ) <> ReqType then Exit( False );
  Result := True;
end;

function TLuaTable.IsField ( const aKey : AnsiString; ReqType : Byte ) : Boolean;
begin
  lua_getfield( FState, -1, PChar( aKey ) );
  Exit( lua_type( FState, -1 ) = ReqType );
end;

{
generic TGLuaSingleEnumerator<T> = object
private
type TLuaReader = function( L : PLua_State; aIndex : Integer ) : T; cdecl;
var
  FState   : PLua_State;
  FCurrent : T;
  FIndex   : Integer;
  FReader  : TLuaReader;
  FField   : ShortInt;
public
  constructor Create( aState : PLua_State; aIndex : Integer; aReader : TLuaReader; aField : ShortInt );
  function MoveNext : Boolean;
public
  property Current : T read FCurrent;
end;

type TLuaVariantEnumerator = object( specialize TGLuaSingleEnumerator<Variant> ) function GetEnumerator : TLuaVariantEnumerator; end;
type TLuaIntegerEnumerator = object( specialize TGLuaSingleEnumerator<Variant> ) function GetEnumerator : TLuaIntegerEnumerator; end;
type TLuaDoubleEnumerator = object( specialize TGLuaSingleEnumerator<Double> ) function GetEnumerator : TLuaDoubleEnumerator; end;
type TLuaAnsiStringEnumerator = object( specialize TGLuaSingleEnumerator<AnsiString> ) function GetEnumerator : TLuaDoubleEnumerator; end;
}

end.

