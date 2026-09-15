{$INCLUDE valkyrie.inc}
// @abstract(LuaSystem class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
//
//  @html <div class="license">
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or (at your
//  option) any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
//  for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this library; if not, write to the Free Software Foundation,
//  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//  @html </div>

//
//  TODO:
//    -- Decouple Lua type system from LuaSystem
//    -- Decouple TNode from LuaSystem
//    -- All lua-based creation should be based on properties!
//    -- use overrides to handle weird properties

unit vlua;
interface
uses classes,
     vlualibrary, vutil, vdebug, vobject, vluastate, vluastack, vluatype, vdf, vgenerics, vluatable, vuid, vrandom;

type
   ELuaException = vlualibrary.ELuaException;
   PLua_State    = vlualibrary.PLua_State;
   PluaL_Reg     = vlualibrary.PluaL_Reg;
   LuaL_Reg      = vlualibrary.luaL_Reg;
   TLuaTable     = vluatable.TLuaTable;
   THookSet      = set of Byte;
   TLuaErrorFunc = procedure( const Message : AnsiString ) of object;
   TLuaPrintFunc = procedure( const Text : AnsiString ) of object;

type
  TLua = class;

  // Owned by TLua; services are borrowed from Runtime and Session.
  TLuaContext = class
  private
    FLua  : TLua;
    FUIDs : TUIDStore;
    FRNG  : TRNG;
  public
    constructor Create( aLua : TLua );
    procedure BindUIDs( aUIDs : TUIDStore );
    procedure BindRNG( aRNG : TRNG );
    class function FromState( aState : PLua_State ) : TLuaContext; static;
    class function RequireRNG( L : PLua_State ) : TRNG; static;
    property Lua  : TLua read FLua;
    property UIDs : TUIDStore read FUIDs;
    property RNG  : TRNG read FRNG;
  end;

type

{ TLuaClassInfo }

 TLuaClassInfo = class(TObject)
    constructor Create( const Proto, Storage : AnsiString );
    procedure RegisterHook( aHookID : Byte; const aHookName : AnsiString );
    procedure RegisterHooks( const aHooks : THookSet; const aHookNames : array of AnsiString );
    function GetHook( HookID : Byte ) : AnsiString;
    function GetHookID( HookName : AnsiString ) : Integer;
  private
    FProto   : AnsiString;
    FStorage : AnsiString;
    FHooks   : array of AnsiString;
    FHookSet : TFlags;
    FHookMax : Byte;
  public
    property Hooks[ HookID : Byte ] : AnsiString read GetHook;
    property Proto   : AnsiString read FProto;
    property Storage : AnsiString read FStorage;
    property HookSet : TFlags     read FHookSet;
    property HookMax : Byte       read FHookMax;
  end;

type TLuaClassMap       = specialize TGObjectHashMap<TLuaClassInfo>;
     TStringBoolMap     = specialize TGHashMap<Boolean>;
     TStringDataFileMap = specialize TGHashMap<TVDataFile>;
     TStringStringMap   = specialize TGHashMap<AnsiString>;
     TIntMap            = specialize TGHashMap<Integer>;

type

{ TLua }

 TLua = class(TVObject)
    // Registers system execution.
    constructor Create( coverState : Plua_State = nil ); reintroduce;
    // Closes system execution.
    destructor Destroy; override;
    // Returns if value is defined without invoking metamethods
    function RawDefined( const aValue : AnsiString ) : Boolean;
    // Returns a value by table path
    function Defined( const Path : AnsiString ) : Boolean;
    // Returns a value by array of const
    function Defined( const Path : array of Const ) : Boolean;
    // Returns a iterator by table path
    function Tables( const Path : AnsiString ) : TLuaTablesEnumerator;
    // Returns a iterator by table path
    function Tables( const Path : array of Const ) : TLuaTablesEnumerator;
    // Returns a iterator by table path
    function ITables( const Path : AnsiString ) : TLuaITablesEnumerator;
    // Returns a iterator by table path
    function ITables( const Path : array of Const ) : TLuaITablesEnumerator;
    // Returns a table by table path
    // Returns a table by table path
    function GetTable( const Path : AnsiString ) : TLuaTable;
    // Returns a table by array of const
    function GetTable( const Path : array of Const ) : TLuaTable;
    // Returns a table size by table path
    function GetTableSize( const Path : AnsiString ) : DWord;
    // Returns a table size by array of const
    function GetTableSize( const Path : array of Const ) : DWord;
    // Returns a value by table path
    function Get( const Path : AnsiString ) : Variant;
    // Returns a value by array of const
    function Get( const Path : array of Const ) : Variant;
    // Returns a value by table path
    function Get( const Path : AnsiString; const DefVal : Variant ) : Variant;
    // Returns a value by array of const
    function Get( const Path : array of Const; const DefVal : Variant ) : Variant;
    // Sets a value by table path
    procedure SetValue( const Path : AnsiString; const Value : Variant );
    // Sets a value by array of const
    procedure SetValue( const Path : array of Const; const Value : Variant );
    // Sets a value by table path
    procedure SetValue( const Path : AnsiString; aObject : TObject );
    // Sets a value by array of const
    procedure SetValue( const Path : array of Const; aObject : TObject );
    // Call a function
    function Call( const Path : array of Const; const Args : array of Const ) : Variant;
    // Call a function
    function Call( const Path : AnsiString; const Args : array of Const ) : Variant;
    // Returns the proto table of the object
    function GetProtoTable( aObj : TObject ) : TLuaTable;
    // Run a hook on a lua object
    function RunHook( Obj : ILuaReferencedObject; HookName : AnsiString; const Params : array of const ) : Variant;
    // Call a function in protected mode -- exceptions will be caught, logged,
    // reported to OnError. False will be returned on Error;
    function ProtectedCall( const Path : array of Const; const Args : array of Const ) : Variant;
    // Call a function in protected mode -- exceptions will be caught, logged,
    // reported to OnError. False will be returned on Error;
    function ProtectedCall( const Path : AnsiString; const Args : array of Const ) : Variant;
    // Run a hook on a lua object in protected mode (see above)
    function ProtectedRunHook( Obj : ILuaReferencedObject; HookName : AnsiString; const Params : array of const ) : Variant;
    // Register table functions
    procedure Register( const libname : AnsiString; const lr : PluaL_Reg );
    // Add empty subtable
    procedure RegisterSubTable( const aTableName, aSubTable : AnsiString );
    // Register Metatables
    procedure RegisterMetaTable( const aTableName : AnsiString; const aIndexFunc, aNewIndexFunc : lua_CFunction );
    // Register Metatables
    procedure RegisterMetaTable( const aTableName, aSubTable : AnsiString; const aIndexFunc, aNewIndexFunc : lua_CFunction );
    // Load raw Lua file
    procedure LoadFile(const FileName : AnsiString);
    // Load Lua code from a stream. WARNING - stream is invalid afterwards!
    procedure LoadStream( IST : TStream; StreamName : AnsiString; Size : DWord ); overload;
    // Load from a Valkyrie Datafile
    procedure LoadStream( DF : TVDataFile; const StreamName : AnsiString); overload;
    // Load from a Valkyrie Datafile
    procedure LoadStream( DF : TVDataFile; const DirName, FileName : AnsiString ); overload;
    // Inform of a recoverable error. No need to log the error here,
    // TLua handles it. Function should be overriden to react on
    // errors that are protected but unrecoverable, or for emiting them to the
    // user.
    // By default does nothing, or runs ErrorFunc if assigned.
    procedure OnError( const Message : AnsiString ); virtual;
    // Registers a lua module for "require" use
    procedure RegisterModule( const ModuleName : AnsiString; DF: TVDataFile );
    // Registers a raw lua module for "require" use. Module Path should end with pathsep,
    // or be empty. RawModules always take priority over compiled ones.
    procedure RegisterModule( const ModuleName, ModulePath: AnsiString);
    // Registers a Lua type.
    procedure RegisterType( AClass : TClass; const ProtoName, StorageName : AnsiString );
    // Return prototype name
    function GetClassInfo( AClass : TClass ) : TLuaClassInfo;
    // Return prototype name
    function GetProtoTable( AClass : TClass ) : AnsiString;
    // Return prototype name
    function GetStorageTable( AClass : TClass ) : AnsiString;
    // Registers an object in Lua space, returns a LuaID
    function RegisterObject( Obj : TObject; aClassName : AnsiString = '' ) : Integer;
    // Unregisters an object
    procedure UnRegisterObject( Obj: ILuaReferencedObject );
    // Returns memory in use (in KBytes)
    function GetMemoryKB : DWord;
    // Returns memory in use (in Bytes)
    function GetMemoryB : DWord;
    // Does a full garbage collection
    procedure CollectGarbage;
    // Sets a print function
    procedure SetPrintFunction( aPrintFunc : TLuaPrintFunc );
    // Print if assigned
    procedure Print( const aText : AnsiString );
    // Execute and print results
    procedure ConsoleExecute( const aCode : AnsiString );
    //
    procedure SetErrorFunc( aErrorFunc : TLuaErrorFunc );
    // Streaming support
    procedure TableToStream( const aPath : AnsiString; aStream : TStream );
    // Streaming support
    procedure TableFromStream( const aPath : AnsiString; aStream : TStream );
  protected
    FContext      : TLuaContext;
    FStack        : TLuaStack;
    FRaw          : PLua_State;
    FState        : TLuaState;
    FErrorFunc    : TLuaErrorFunc;
    FPrintFunc    : TLuaPrintFunc;
    FModuleNames  : TStringBoolMap;
    FDataFiles    : TStringDataFileMap;
    FRawModules   : TStringStringMap;
    FClassMap     : TLuaClassMap;
    FCallDefVal   : Variant;
    FDefines      : TIntMap;
  public
    property CallDefaultResult : Variant     read FCallDefVal write FCallDefVal;
    property Context : TLuaContext read FContext;
    property Raw : PLua_State                read FRaw;
    property Stack : TLuaStack               read FStack;
    property State : TLuaState               read FState;
    property ErrorFunc : TLuaErrorFunc write SetErrorFunc;
    property PrintFunc : TLuaPrintFunc read FPrintFunc;
    property ModuleNames : TStringBoolMap    read FModuleNames;
    property Defines : TIntMap               read FDefines;

  private
    // Uses the caller's stack, which may belong to a coroutine.
    function PrintValue( L : PLua_State; aIndex : Integer; aIndent : Word = 0; aPrefix : AnsiString = '' ) : Word;
    // Pushes the path, leaves the last element on top, and the table below it
    // raises on failure of path.
    procedure GetPath( const Path : AnsiString );
    // Pushes the path, leaves the last element on top, and the table below it
    // raises on failure of path.
    procedure GetPath( const Path : array of Const );
    // Convert path to string
    function PathToString( const Path : array of Const ) : AnsiString;
    // Deep copy of lua object with copying of __ptr field
    procedure DeepPointerCopy( Index : Integer; Obj : Pointer );
  end;


implementation

uses variants, sysutils, strutils, math, vluaext;

// Its address is a private registry key, not a published context pointer.
var LuaContextKey : Byte;

constructor TLuaContext.Create( aLua : TLua );
begin
  inherited Create;
  FLua := aLua;
end;

procedure TLuaContext.BindUIDs( aUIDs : TUIDStore );
begin
  FUIDs := aUIDs;
end;

procedure TLuaContext.BindRNG( aRNG : TRNG );
begin
  FRNG := aRNG;
end;

class function TLuaContext.FromState( aState : PLua_State ) : TLuaContext;
begin
  lua_pushlightuserdata( aState, @LuaContextKey );
  lua_rawget( aState, LUA_REGISTRYINDEX );
  Result := TLuaContext( lua_touserdata( aState, -1 ) );
  lua_pop( aState, 1 );
end;

class function TLuaContext.RequireRNG( L : PLua_State ) : TRNG;
var iContext : TLuaContext;
begin
  iContext := FromState( L );
  if iContext = nil then
    luaL_error( L, 'Lua context is not registered' );
  Result := iContext.RNG;
  if Result = nil then
    luaL_error( L, 'RNG is not bound to this Lua context' );
end;

const BlueprintTypes : array[-1..8] of PChar = ( 'TANY', 'TNIL', 'TBOOL', 'TLUSER', 'TNUMBER', 'TSTRING', 'TTABLE', 'TFUNC', 'TUSER', 'TTHREAD' );

function TLua.PrintValue( L : PLua_State; aIndex : Integer; aIndent : Word; aPrefix : AnsiString ) : Word;
var iLines : Byte;
begin
  aIndex := lua_absindex(L,aIndex);
  aPrefix := StringOfChar(' ',aIndent)+aPrefix;
  case lua_type(L,aIndex) of
    LUA_TNIL           : Print(aPrefix+'{Rnil}');
    LUA_TBOOLEAN       : if lua_toboolean(L,aIndex) then Print(aPrefix+'{gtrue}') else Print(aPrefix+'{gfalse}');
    LUA_TLIGHTUSERDATA : Print(aPrefix+'{blightuserdata({^0x'+hexstr(lua_touserdata(L,aIndex))+'})}');
    LUA_TNUMBER        : Print(aPrefix+'{L'+lua_tostring(L,aIndex)+'}');
    LUA_TSTRING        : Print(aPrefix+'"{G'+lua_tostring(L,aIndex)+'}"');
    LUA_TFUNCTION      : Print(aPrefix+'{yfunction}');
    LUA_TUSERDATA      : Print(aPrefix+'{yuserdata}');
    LUA_TTHREAD        : Print(aPrefix+'{ythread}');
    LUA_TTABLE         :
      begin
        Print(aPrefix+'{ytable} = [');
        aIndent += 2;
        iLines := 2;
        lua_pushnil(L);
        while lua_next(L, aIndex) <> 0 do
        begin
          // Key at -2, value at -1.
          if lua_isnumber( L, -2 ) then
            iLines += PrintValue( L, -1, aIndent, IntToStr(lua_tointeger( L, -2 ))+' = ')
          else
            iLines += PrintValue( L, -1, aIndent, lua_tostring( L, -2 )+' = ');
          // remove value, keep key
          lua_pop(L, 1);
          if iLines > 10 then
          begin
            Print(StringOfChar(' ',aIndent)+'...');
            lua_pop(L, 1);
            break;
          end;
        end;
        if iLines <= 8 then Print(StringOfChar(' ',aIndent-2)+']');
        Exit(iLines);
      end;
  end;
  Exit(1);
end;

function lua_valkyrie_print( L : PLua_State ) : Integer; cdecl;
var iIndex : Integer;
    iLua   : TLua;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  if Assigned( iLua.FPrintFunc ) then
  begin
    iIndex := lua_gettop(L);
    if iIndex <= 0 then Exit(0);
    for iIndex := 1 to lua_gettop(L) do
      iLua.PrintValue( L, iIndex );
  end;
  Result := 0;
end;

{ TLua }

function lua_valkyrie_require( L : PLua_State ) : Integer; cdecl;
var iLua      : TLua;
    iArg      : AnsiString;
    iModule   : AnsiString;
    iPath     : AnsiString;
    iFileName : AnsiString;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  if lua_gettop(L) <> 1 then iLua.OnError('Require has wrong amount of parameters!');
  iArg := lua_tostring( L, 1 );

  if iLua.FModuleNames.Exists(iArg) then Exit(0);

  iModule := ExtractDelimited( 1, iArg, [':'] );
  iPath := ExtractFilePath( iArg );
  if iModule <> '' then
    Delete( iPath, 1, Length( iModule ) + 1 );

  if (Length(iPath) > 0) and (iPath[Length(iPath)] = '/') then Delete(iPath,Length(iPath),1);
  iFileName := ExtractFileName( iArg ) + '.lua';

  if Pos(':', iFileName) > 0 then
    Delete( iFileName, 1, Pos(':', iFileName) );

  Log('LuaRequire( Module "'+iModule+'", Path "'+iPath+'", FileName "'+iFileName+'")');

  if not iLua.FRawModules.Exists(iModule) then
  begin
    if not iLua.FDataFiles.Exists(iModule) then
      raise ELuaException.Create('require : Module "'+iModule+'" not found!');
    iLua.LoadStream( iLua.FDataFiles[iModule], iPath, iFileName );
  end
  else
  begin
    if iPath <> '' then
      iPath := iLua.FRawModules[ iModule ] + iPath + DirectorySeparator + iFileName
    else
      iPath := iLua.FRawModules[ iModule ] + iFileName;
    if not FileExists(iPath) then
      raise ELuaException.Create('require : File "'+iPath+'" not found!');
    iLua.LoadFile( iPath );
  end;

  iLua.FModuleNames[ iArg ] := True;
  Exit( 0 );
end;

function lua_core_log( L: Plua_State ): Integer; cdecl;
var iState : TLuaStack;
begin
  iState.Init( L );
  Log( iState.ToString(1) );
  Result := 0;
end;

function lua_core_warning( L: Plua_State ): Integer; cdecl;
var iState : TLuaStack;
begin
  iState.Init( L );
  Log( LOGWARN, iState.ToString(1) );
  Result := 0;
end;

function core_make_id( const aName : AnsiString ) : AnsiString;
const ValidChars = ['a'..'z','_','-','A'..'Z','0','1'..'9'];
var iName  : AnsiString;
    iCount : DWord;
begin
  iName := LowerCase( aName );
  for iCount := 1 to Length(iName) do
    if not (iName[iCount] in ValidChars) then
      iName[iCount] := '_';
  Result := iName;
end;

function blueprint_exists ( L: Plua_State; IINDEX : Integer ) : Boolean;
begin
  lua_getglobal( L, 'core' );
  lua_getfield( L, -1, 'blueprints' );
  lua_pushvalue( L, IINDEX );
  lua_rawget( L, -2 );
  blueprint_exists := lua_istable( L, -1 );
  lua_pop( L, 3 );
end;

procedure push_blueprint( L: Plua_State; IINDEX : Integer );
begin
  lua_getglobal( L, 'core' );
  lua_getfield( L, -1, 'blueprints' );
  lua_pushvalue( L, IINDEX );
  lua_rawget( L, -2 );
  if not lua_istable( L, -1 ) then
    luaL_error( L, 'core.blueprint "%s" doens''t exist (yet?)!', lua_tolstring( L, IINDEX, nil ) );
  lua_replace( L, -3 );
  lua_pop( L, 2 );
end;

function lua_core_apply_blueprint ( L: Plua_State): Integer; cdecl; forward;

// Returns true if pushes modified value
function lua_core_check_type_raw( L: Plua_State; IIDENT, IFIELD, IVALUE, ITYPE : Integer ) : Boolean;
begin
  IIDENT   := lua_absindex( L, IIDENT );
  IVALUE   := lua_absindex( L, IVALUE );
  ITYPE    := lua_absindex( L, ITYPE );
  IFIELD   := lua_absindex( L, IFIELD );
  case lua_type( L, ITYPE ) of
    LUA_TFUNCTION :
      begin
        lua_pushvalue( L, ITYPE );  // type function
        lua_pushvalue( L, IIDENT ); // identifier
        lua_pushvalue( L, IFIELD ); // field name
        lua_pushvalue( L, IVALUE ); // field value
        lua_call( L, 3, 1 );
        if lua_isnoneornil( L, -1 )
          then lua_pop( L, 1 )
          else Exit( True );
      end;
    LUA_TSTRING :
      begin
        if (lua_type( L, IVALUE ) <> LUA_TTABLE ) then
          luaL_error( L, 'LUA: "%s.%s" - type mismatch, table of blueprint "%s" expected, %s found!', lua_tolstring( L, IIDENT, nil ), lua_tolstring( L, IFIELD, nil ), lua_tolstring( L, ITYPE, nil ), lua_typename( L, lua_type( L, IVALUE ) ) );
        lua_pushcfunction( L, @lua_core_apply_blueprint );
        lua_pushvalue( L, IVALUE );
        lua_pushvalue( L, ITYPE );
        lua_pushvalue( L, IIDENT );
        lua_pushstring( L, '.' );
        lua_pushvalue( L, IFIELD );
        lua_concat( L, 3 );
        lua_call( L, 3, 0 );
      end;
    LUA_TNUMBER :
        if (lua_tointeger( L, ITYPE ) <> lua_type( L, IVALUE )) and (lua_tointeger( L, ITYPE ) > 0) then
          luaL_error( L, 'LUA: "%s.%s" - type mismatch, %s expected, %s found!', lua_tolstring( L, IIDENT, nil ), lua_tolstring( L, IFIELD, nil ), lua_typename( L, lua_tointeger( L, ITYPE ) ), lua_typename( L, lua_type( L, IVALUE ) ) );
  end;
  Exit( False );
end;

function lua_core_make_id(L: Plua_State): Integer; cdecl;
var State  : TLuaStack;
begin
  State.Init(L);
  State.Push( core_make_id( State.ToString(1) ) );
  Result := 1;
end;

procedure lua_core_apply_blueprint_values_raw(L: Plua_State; IBASE, IPROTO, ISET, IIDENT : Integer );
var mandatory : boolean;
    nested    : boolean;
    present   : boolean;
    IKEY      : Integer;
    IVALUE    : Integer;
begin
  IBASE  := lua_absindex( L, IBASE );
  IPROTO := lua_absindex( L, IPROTO );
  ISET   := lua_absindex( L, ISET );
  IIDENT := lua_absindex( L, IIDENT );

  lua_pushnil(L);
  while lua_next( L, IPROTO ) <> 0 do
  begin
    // Key -2, Value -1
    IKEY      := lua_absindex( L, -2 );
    IVALUE    := lua_absindex( L, -1 );

    // Base[Key]
    lua_pushvalue( L, IKEY );
    lua_rawget( L, IBASE );
    present   := not lua_isnil( L, -1 );
    lua_pop( L, 1 );

    if lua_type( L, IVALUE ) = LUA_TTABLE then
    begin
      // Value[1]
      lua_rawgeti( L, IVALUE, 1 );
      mandatory := lua_toboolean( L, -1 );
      nested    := lua_type( L, -1 ) = LUA_TTABLE;
      lua_pop( L, 1 );

      if not present then
      begin
        if mandatory then luaL_error( L, 'LUA: %s has no required field "%s"!', lua_tolstring( L, IIDENT, nil ), lua_tolstring( L, -2, nil ) )
        else
        begin
          lua_pushvalue( L, IKEY ); // push Key
          lua_rawgeti( L, IVALUE, 3 ); // Value[3]
          if lua_type( L, -1 ) = LUA_TTABLE then
          begin
            vlua_shallowcopy( L, -1 );
            lua_replace( L, -2 );
          end;
          lua_rawset( L, IBASE ); // Base[Key] =
        end;
      end;

      if nested then
      begin
        lua_rawgeti( L, IVALUE, 1 ); // v[1]
        lua_pushvalue( L, IKEY );  // Key
        lua_rawget( L, IBASE );  // Base[Key]
        lua_rawget( L, -2 );     // v[1][Base[Key]]
        lua_replace( L, -2 );    // down to 1 stack
        if not lua_isnil( L, -1 ) then
          lua_core_apply_blueprint_values_raw( L, IBASE, -1, ISET, IIDENT );
        lua_pop( L, 1 );
      end
      else
      if present then
      begin
        lua_rawgeti( L, IVALUE, 2 ); // Value[2]
        lua_pushvalue( L, IKEY );  // Key
        lua_rawget( L, IBASE );  // Base[Key]
        if lua_core_check_type_raw( L, IIDENT, IKEY, -1, -2 ) then
        begin
          lua_pushvalue( L, IKEY );  // Key
          lua_insert( L, -2 );
          lua_rawset( L, IBASE );
        end;
        lua_pop( L, 2 );
      end;
    end
    else
    begin
      if present then luaL_error( L, 'LUA: %s - field "%s" cannot be redefined!', lua_tolstring( L, IIDENT, nil ), lua_tolstring( L, IKEY, nil ) );
      // non-table entries get forced copied
      lua_pushvalue( L, IKEY ); // push Key
      lua_pushvalue( L, IVALUE ); // push Value
      lua_rawset( L, IBASE ); // Base[Key] =
    end;
    lua_pushvalue( L, IKEY ); // Key
    lua_pushnil( L );
    lua_rawset( L, ISET );  // Set[Key] = nil
    lua_pop( L, 1 );
  end;
end;

function lua_core_apply_blueprint_values(L: Plua_State): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );  // base
  luaL_checktype( L, 2, LUA_TTABLE );  // prototype
  luaL_checktype( L, 3, LUA_TTABLE );  // set
  luaL_checktype( L, 4, LUA_TSTRING ); // ident
  lua_settop( L, 4 );
  lua_core_apply_blueprint_values_raw( L, 1, 2, 3, 4 );
  Result := 0;
end;

procedure lua_core_apply_blueprint_raw(L: Plua_State; IBASE, IPROTO, IIDENT : Integer );
var ISET : integer;
begin
  IBASE  := lua_absindex( L, IBASE );
  IPROTO := lua_absindex( L, IPROTO );
  IIDENT := lua_absindex( L, IIDENT );
  vlua_table_tokeyset( L, IBASE );
  ISET   := lua_absindex( L, -1 );

  lua_core_apply_blueprint_values_raw( L, IBASE, IPROTO, ISET, IIDENT );

  lua_pushnil(L);
  while lua_next( L, ISET ) <> 0 do
  begin
    if (lua_type(L, -2) = LUA_TSTRING) and (Copy(lua_tolstring(L, -2, nil), 1, 2) = '__') then
    begin
      lua_pop(L, 1);
      continue;
    end;
    luaL_error( L, 'LUA: %s has unknown field "%s"!', lua_tolstring( L, 3, nil ), lua_tolstring( L, -2, nil ) );
    lua_pop(L, 1);
  end;
  lua_pop(L, 1);
end;

function lua_core_apply_blueprint ( L: Plua_State): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );  // base
  if lua_type( L, 2 ) = LUA_TSTRING then
  begin
    lua_getglobal( L, 'core' );
    lua_getfield( L, -1, 'blueprints' );
    lua_pushvalue( L, 2 );
    lua_rawget( L, -2 );
    lua_replace( L, 2 );
  end;
  luaL_checktype( L, 2, LUA_TTABLE );  // prototype
  luaL_checktype( L, 3, LUA_TSTRING ); // ident
  lua_settop( L, 3 );
  lua_core_apply_blueprint_raw( L, 1, 2, 3 );
  Result := 1;
end;

function lua_core_register_blueprint_impl(L: Plua_State): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  lua_getglobal( L, 'core' );
  lua_getfield( L, -1, 'blueprints' );

  if lua_isnil( L, lua_upvalueindex(2) ) then
  begin
    lua_pushvalue( L, lua_upvalueindex(1));
    lua_pushvalue( L, 1 );
    lua_rawset( L, -3 );
  end
  else
  begin
    lua_pushvalue( L, lua_upvalueindex(1));
    lua_pushvalue( L, lua_upvalueindex(2));
    lua_rawget( L, -3 );
    vlua_shallowcopy( L, -1 );
    lua_replace( L, -2 );
    vlua_shallowmerge( L, 1 );
    lua_rawset( L, -3 );
  end;

  lua_pop( L, 2 );
  Exit( 0 );
end;

function lua_core_register_blueprint(L: Plua_State): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TSTRING );
  if blueprint_exists( L, 1 ) then luaL_error( L, 'core.blueprint "%s" already registered!', lua_tolstring( L, 1, nil ) );
  if lua_gettop( L ) > 1 then
  begin
    luaL_checktype( L, 2, LUA_TSTRING );
    if not blueprint_exists( L, 2 ) then luaL_error( L, 'core.blueprint "%s" doens''t exist (yet?)!', lua_tolstring( L, 2, nil ) );
  end
  else
    lua_pushnil( L );
  lua_pushcclosure(L, @lua_core_register_blueprint_impl, 2);
  Result := 1;
end;

procedure lua_core_register_callback_impl( L : Plua_State; INAME : Integer; const aBlueprintName : PChar );
begin
  lua_getglobal( L, 'core' );
  lua_getfield( L, -1, 'blueprints' );
  lua_getfield( L, -1, aBlueprintName );
  if not lua_istable( L, -1 ) then
    luaL_error( L, 'core.register_callback - blueprint "%s" doesn''t exist!', aBlueprintName );
  // create spec table { false, LUA_TFUNCTION }
  lua_createtable( L, 2, 0 );
  lua_pushboolean( L, false );
  lua_rawseti( L, -2, 1 );
  lua_pushinteger( L, LUA_TFUNCTION );
  lua_rawseti( L, -2, 2 );
  // blueprint[func_name] = spec
  lua_setfield( L, -2, lua_tolstring( L, INAME, nil ) );
  lua_pop( L, 3 ); // blueprint, blueprints and core
end;

function lua_core_register_callback( L : Plua_State ) : Integer; cdecl;
var i, n : Integer;
begin
  luaL_checktype( L, 1, LUA_TSTRING );
  if lua_type( L, 2 ) = LUA_TSTRING then
    lua_core_register_callback_impl( L, 1, lua_tolstring( L, 2, nil ) )
  else if lua_type( L, 2 ) = LUA_TTABLE then
  begin
    n := lua_objlen( L, 2 );
    if n = 0 then Exit( 0 );
    for i := 1 to n do
    begin
      lua_rawgeti( L, 2, i );
      if lua_type( L, -1 ) <> LUA_TSTRING then
        luaL_error( L, 'core.register_callback - blueprint array entry %d is not a string!', i );
      lua_core_register_callback_impl( L, 1, lua_tolstring( L, -1, nil ) );
      lua_pop( L, 1 );
    end;
  end
  else
    luaL_error( L, 'core.register_callback - second parameter must be a string or array of strings!' );
  // One callback ID per name, regardless of the number of blueprints.
  lua_getglobal( L, 'core' );
  lua_getfield( L, -1, 'callbacks' );
  n := lua_objlen( L, -1 );
  for i := 1 to n do
  begin
    lua_rawgeti( L, -1, i );
    if lua_rawequal( L, 1, -1 ) then
    begin
      lua_pop( L, 3 );
      Exit( 0 );
    end;
    lua_pop( L, 1 );
  end;
  lua_pushvalue( L, 1 );
  lua_rawseti( L, -2, n + 1 );
  lua_pop( L, 2 );
  Result := 0;
end;

function lua_core_register(L: Plua_State): Integer; cdecl; forward;

function lua_core_create_constructor_impl( L : PLua_State ) : Integer; cdecl;
var iLua   : TLua;
    iIdent : AnsiString;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  luaL_checktype( L, 1, LUA_TTABLE );

  lua_getfield( L, 1, 'inherit' );
  if not lua_isnil( L, -1 ) then
  begin
    if lua_type( L, -1 ) <> LUA_TSTRING then
      luaL_error( L, 'inherit field must be a string!' );

    lua_pushvalue( L, lua_upvalueindex( 2 ) );
    lua_pushvalue( L, -2 );
    lua_rawget( L, -2 );

    if lua_isnil( L, -1 ) then
      luaL_error( L, 'Cannot inherit from "%s" - not found in storage!', lua_tolstring( L, -3, nil ) );

    lua_getfield( L, -1, '__source' );
    if lua_isnil( L, -1 ) then
      luaL_error( L, 'Parent entry "%s" has no __source field!', lua_tolstring( L, -4, nil ) );

    vlua_deepcopy( L, -1 );

    lua_pushnil( L );
    lua_setfield( L, -2, 'id' );
    lua_pushnil( L );
    lua_setfield( L, -2, 'nid' );

    lua_pushvalue( L, -3 );
    lua_setfield( L, -2, '__inherited' );

    vlua_shallowmerge( L, 1 );
    lua_replace( L, 1 );
    lua_pop( L, 4 );

    lua_pushnil( L );
    lua_setfield( L, 1, 'inherit' );
  end
  else
    lua_pop( L, 1 );

  vlua_deepcopy( L, 1 );
  lua_setfield( L, 1, '__source' );

  lua_pushvalue( L, lua_upvalueindex( 1 ) ); // id
  iIdent := vlua_tostring( L, -1 );
  if iLua.Defines.Exists( iIdent ) then
    luaL_error( L, 'Redefinition of id "%s"!', lua_tolstring( L, -1, nil ) );
  lua_setfield( L, 1, 'id' );

  lua_getfield( L, 1, 'blueprint' );
  if lua_isnil( L, -1 ) and (not lua_isnoneornil( L, lua_upvalueindex( 3 ) )) then // blueprint
  begin
    lua_pushvalue( L, lua_upvalueindex( 3 ) );
    lua_replace( L, -2 );
  end;
  if lua_isnil( L, -1 ) then // storage.__blueprint
  begin
    lua_getfield( L, lua_upvalueindex( 2 ), '__blueprint' );
    lua_replace( L, -2 );
  end;
  if not lua_isnil( L, -1 ) then
  begin
    lua_getfield( L, lua_upvalueindex( 2 ), '__name' ); // storage.__name
    iIdent := vlua_tostring( L, -1 ) + '[' + iIdent + ']';
    lua_pop( L, 1 );

    lua_pushcfunction( L, @lua_core_apply_blueprint );
    lua_pushvalue( L, 1 );
    lua_pushvalue( L, -3 );
    lua_pushstring( L, PChar(iIdent) );
    lua_call( L, 3, 0 );
    lua_pop( L, 1 );
  end;

  lua_pushcfunction( L, @lua_core_register );
  lua_pushvalue( L, lua_upvalueindex( 2 ) );
  lua_pushvalue( L, 1 );
  lua_call( L, 2, 0 );

  if (not lua_isnoneornil( L, lua_upvalueindex( 4 ) )) then // constructor
  begin
    lua_pushvalue( L, lua_upvalueindex( 4 ) ); // constructor
    lua_pushvalue( L, 1 );
    lua_call( L, 1, 0 );
  end;

  lua_pushvalue( L, lua_upvalueindex( 1 ) ); // id
  result := 1;
end;

function lua_core_create_constructor_closure(L: Plua_State): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TSTRING );
  if lua_gettop( L ) > 1 then
  begin
    lua_settop( L, 2 );
    luaL_checktype( L, 2, LUA_TSTRING );
    lua_pushvalue( L, lua_upvalueindex( 1 ) );
    lua_insert( L, -2 );
  end
  else
  begin
    lua_settop( L, 1 );
    lua_pushvalue( L, lua_upvalueindex( 1 ) );
    lua_pushvalue( L, lua_upvalueindex( 2 ) );
  end;
  lua_pushvalue( L, lua_upvalueindex( 3 ) );
  lua_settop( L, 4 );

  lua_pushcclosure(L, @lua_core_create_constructor_impl, 4);
  Result := 1;
end;

function lua_core_create_constructor(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 1 ) = LUA_TSTRING then
  begin
    lua_getglobal( L, lua_tostring( L, 1 ) );
    lua_replace( L, 1 );
  end;
  luaL_checktype( L, 1, LUA_TTABLE );
  if not lua_isnoneornil( L, 2 ) then
  begin
    luaL_checktype( L, 2, LUA_TSTRING );
  end;
  if not lua_isnoneornil( L, 3 ) then
  begin
    luaL_checktype( L, 3, LUA_TFUNCTION );
  end;
  lua_settop( L, 3 );
  lua_pushcclosure(L, @lua_core_create_constructor_closure, 3);
  Result := 1;
end;

function lua_core_array_register(L: Plua_State): Integer; cdecl; forward;

function lua_core_create_array_constructor_impl(L: Plua_State): Integer; cdecl;
var ident     : ansistring;
    blueprint : Boolean;
begin
  blueprint := True;
  luaL_checktype( L, 1, LUA_TTABLE );

  lua_getfield( L, lua_upvalueindex( 1 ), '__blueprint' );
  if lua_type( L, -1 ) = LUA_TBOOLEAN then
    blueprint := lua_toboolean( L, -1 );
  lua_pop( L, 1 );

  if blueprint then
  begin
    lua_getfield( L, 1, 'blueprint' );

    if lua_isnil( L, -1 ) and (not lua_isnoneornil( L, lua_upvalueindex( 2 ) )) then // blueprint
    begin
      lua_pushvalue( L, lua_upvalueindex( 2 ) );
      lua_replace( L, -2 );
    end;

    if lua_isnil( L, -1 ) then // storage.__blueprint
    begin
      lua_getfield( L, lua_upvalueindex( 1 ), '__blueprint' );
      lua_replace( L, -2 );
    end;

    if (not lua_isnil( L, -1 )) and (lua_type( L, -1) <> LUA_TBOOLEAN) then
    begin
      lua_getfield( L, lua_upvalueindex( 1 ), '__name' ); // storage.__name
      ident := vlua_tostring( L, -1 ) + '[' + IntToStr(lua_objlen(L,lua_upvalueindex( 1 ))+1) + ']';
      lua_pop( L, 1 );

      lua_pushcfunction( L, @lua_core_apply_blueprint );
      lua_pushvalue( L, 1 );
      lua_pushvalue( L, -3 );
      lua_pushstring( L, PChar(ident) );
      lua_call( L, 3, 0 );
      lua_pop( L, 1 );
    end;
  end;

  lua_pushcfunction( L, @lua_core_array_register );
  lua_pushvalue( L, lua_upvalueindex( 1 ) );
  lua_pushvalue( L, 1 );
  lua_call( L, 2, 1 );

  if (not lua_isnoneornil( L, lua_upvalueindex( 3 ) )) then // constructor
  begin
    lua_pushvalue( L, lua_upvalueindex( 3 ) ); // constructor
    lua_pushvalue( L, 1 );
    lua_call( L, 1, 0 );
  end;

  result := 1;
end;

function lua_core_create_array_constructor(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 1 ) = LUA_TSTRING then
  begin
    lua_getglobal( L, lua_tostring( L, 1 ) );
    lua_replace( L, 1 );
  end;
  luaL_checktype( L, 1, LUA_TTABLE );
  if not lua_isnoneornil( L, 2 ) then
  begin
    if lua_type( L, 2 ) <> LUA_TBOOLEAN then
      luaL_checktype( L, 2, LUA_TSTRING );
  end;
  if not lua_isnoneornil( L, 3 ) then
  begin
    luaL_checktype( L, 3, LUA_TFUNCTION );
  end;
  lua_settop( L, 3 );
  lua_pushcclosure(L, @lua_core_create_array_constructor_impl, 3);
  Result := 1;
end;

function lua_core_register_storage(L: Plua_State): Integer; cdecl;
var blueprint : boolean;
    constr    : boolean;
begin
  blueprint := false;
  constr    := false;
  luaL_checktype( L, 1, LUA_TSTRING );
  if not lua_isnoneornil( L, 2 ) then
  begin
    luaL_checktype( L, 2, LUA_TSTRING );
    blueprint := true;
  end;
  if not lua_isnoneornil( L, 3 ) then
  begin
    luaL_checktype( L, 3, LUA_TFUNCTION );
    constr := true;
  end;
  lua_settop( L, 3 );
  lua_pushvalue( L, 1 );
  lua_rawget_global( L );
  if not lua_isnil( L, -1 ) then luaL_error( L, 'storage "%s" already registered!', lua_tolstring( L, lua_upvalueindex(1), nil ) );
  lua_pop( L, 1 );

  lua_newtable( L );
  lua_pushvalue( L, 1 ); // name
  lua_pushvalue( L, -2 ); // duplicate table
  lua_pushvalue( L, 1 ); // name
  lua_setfield( L, -2, '__name' );
  if blueprint then
  begin
    lua_pushvalue( L, 2 );
    lua_setfield( L, -2, '__blueprint' );
  end;
  lua_rawset_global( L );

  lua_pushcfunction( L, @lua_core_create_constructor );
  lua_pushvalue( L, -2 ); // storage
  if blueprint
    then lua_pushvalue( L, 2 )
    else lua_pushnil( L );
  if constr
    then lua_pushvalue( L, 3 )
    else lua_pushnil( L );
  lua_call( L, 3, 1 );
  Result := 1;
end;

function lua_core_register_array_storage(L: Plua_State): Integer; cdecl;
var blueprint : boolean;
    constr    : boolean;
begin
  blueprint := false;
  constr    := false;
  luaL_checktype( L, 1, LUA_TSTRING );
  if not lua_isnoneornil( L, 2 ) then
  begin
    if lua_type( L, 2 ) <> LUA_TBOOLEAN then
      luaL_checktype( L, 2, LUA_TSTRING );
    blueprint := true;
  end;
  if not lua_isnoneornil( L, 3 ) then
  begin
    luaL_checktype( L, 3, LUA_TFUNCTION );
    constr := true;
  end;

  //  lua_getglobal( L, lua_tostring( L, 1 ) ); // cant - meta called
  lua_pushvalue( L, 1 );
  lua_rawget_global( L );
  if not lua_isnil( L, -1 ) then luaL_error( L, 'storage "%s" already registered!', lua_tolstring( L, lua_upvalueindex(1), nil ) );
  lua_pop( L, 1 );

  lua_newtable( L );
  lua_pushvalue( L, 1 ); // name
  lua_pushvalue( L, -2 ); // duplicate table
  lua_pushvalue( L, 1 ); // name
  lua_setfield( L, -2, '__name' );
  if blueprint then
  begin
    lua_pushvalue( L, 2 );
    lua_setfield( L, -2, '__blueprint' );
  end;
  lua_rawset_global( L );

  lua_pushcfunction( L, @lua_core_create_array_constructor );
  lua_pushvalue( L, -2 ); // storage
  if blueprint
    then lua_pushvalue( L, 2 )
    else lua_pushnil( L );
  if constr
    then lua_pushvalue( L, 3 )
    else lua_pushnil( L );
  lua_call( L, 3, 1 );
  Result := 1;
end;

function lua_core_array_register(L: Plua_State): Integer; cdecl;
var Count : Integer;
begin
  // storage.__counter++
  lua_pushstring( L, '__counter' );
  lua_pushvalue( L, -1 );
  lua_rawget( L, 1 );
  Count := 0;
  if not lua_isnil( L, -1 ) then
    Count := lua_tointeger( L, -1 );
  lua_pop( L, 1 );
  Inc( Count );
  lua_pushinteger( L, Count );
  lua_rawset( L, 1 );

  // element.nid = __counter
  lua_pushstring( L, 'nid' );
  lua_pushinteger( L, Count );
  lua_rawset( L, 2 );

  // storage[ __counter ] = element
  lua_pushinteger( L, Count );
  lua_pushvalue( L, 2 );
  lua_rawset( L, 1 );

  // return nid
  lua_pushinteger( L, Count );
  Result := 1;
end;

function lua_core_register( L : PLua_State ) : Integer; cdecl;
var iLua       : TLua;
    iName, iID : AnsiString;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  lua_core_array_register(L);

  // iName := element.name or "error"
  lua_pushstring( L, 'name' );
  lua_rawget( L, 2 );
  if lua_isnil( L, -1 ) then
    iName := 'error'
  else
    iName := lua_tostring( L, -1 );
  lua_pop( L, 1 );

  // element.id = element.id or core.make_id( element.name )
  lua_pushstring( L, 'id' );
  lua_rawget( L, 2 );
  if lua_isnil( L, -1 ) then
    if iName = 'error' then raise ELuaException.Create('Element without ID nor name!')
    else
    begin
      iID := core_make_id( iName );
      lua_pushstring( L, PChar(iID) );
      lua_pushstring( L, 'id' );
      lua_pushvalue( L, -2 );
      lua_rawset( L, 2 );
    end
  else
    iID := lua_tostring( L, -1 );
  lua_pop( L, 1 );

  // storage[element.id] = element
  lua_pushstring( L, PChar(iID) );
  lua_pushvalue( L, 2 );
  lua_rawset( L, 1 );

  // core.define( element.id, element.nid )
  iLua.FDefines[ iID ] := lua_tointeger( L, 3 );

  // return id
  lua_pushstring( L, PChar(iID) );
  Result := 1;
end;

function lua_core_unregister( L : PLua_State ) : Integer; cdecl;
var iLua  : TLua;
    iID   : Integer;
    iName : AnsiString;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  luaL_checktype( L, 1, LUA_TTABLE );

  if lua_type( L, 2 ) = LUA_TSTRING then
  begin
    iName  := vlua_tostring(L,2);
    iID := iLua.Defines[ iName ];
    iLua.Defines.Remove( iName );
    lua_pushvalue( L, 2 );
    lua_pushnil( L );
    lua_rawset( L, 1 );
    lua_pushinteger( L, iID );
    lua_pushnil( L );
    lua_rawset( L, 1 );
    Exit(0);
  end;

  lua_pushnil(L);
  while lua_next( L, 1 ) <> 0 do
  begin
    if lua_type( L, -2 ) = LUA_TSTRING then
      iLua.Defines.Remove(vlua_tostring(L,-2));
    lua_pushvalue( L, -2 );
    lua_pushnil( L );
    lua_rawset( L, 1 );
    lua_pop(L, 1);
  end;
  Result := 0;
end;

function lua_core_define( L : PLua_State ) : Integer; cdecl;
var iLua   : TLua;
    iState : TLuaStack;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  iState.Init( L );
  iLua.FDefines[ iState.ToString(1) ] := iState.ToInteger(2);
  Result := 0;
end;

function lua_core_undefine( L : PLua_State ) : Integer; cdecl;
var iLua   : TLua;
    iState : TLuaStack;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  iState.Init(L);
  iLua.Defines.Remove(iState.ToString(1));
  Result := 0;
end;

function lua_core_declare(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) < 1 then Exit(0);
  if lua_gettop(L) = 1 then lua_pushboolean( L, false );
  lua_settop( L, 2 );
  lua_rawset_global( L );
  Result := 0;
end;

function lua_core_register_hook(L: Plua_State): Integer; cdecl;
begin
  luaL_checktype( L, 1, LUA_TTABLE );
  luaL_checktype( L, 2, LUA_TSTRING );
  luaL_checktype( L, 3, LUA_TFUNCTION );

  lua_pushstring( L, '__hooks' );
  lua_rawget( L, 1 );
  if lua_isnil( L, -1 ) then
  begin
    lua_newtable( L );
    lua_pushstring( L, '__hooks' );
    lua_pushvalue( L, -2 );
    lua_rawset( L, 1 );
  end;

  lua_pushvalue( L, 2 );
  lua_pushvalue( L, 3 );
  lua_rawset( L, -3 );
  result := 0;
end;

function lua_core_set_rseed( L : PLua_State ) : Integer; cdecl;
var iRNG : TRNG;
begin
  if lua_gettop(L) < 1 then Exit(0);
  iRNG := TLuaContext.RequireRNG( L );
  iRNG.SetSeed( DWord( lua_tointeger( L, 1 ) ) );
  Result := 0;
end;

function lua_core_iif(L: Plua_State): Integer; cdecl;
begin
  if lua_toboolean( L, 1 )
    then lua_settop( L, 2 )
    else lua_settop( L, 3 );
  Result := 1;
end;

function lua_core_create_seq_function_closure(L: Plua_State): Integer; cdecl;
var i, fi, args, fc : Integer;
begin
  fc   := lua_tointeger( L, lua_upvalueindex( 1 ) );
  args := lua_gettop( L );
  for fi := 1 to fc do
  begin
    lua_pushvalue( L, lua_upvalueindex( fi + 1 ) );
    for i := 1 to args do
      lua_pushvalue( L, i );
    lua_call( L, args, 0 );
  end;
  result := 0;
end;

function lua_core_create_seq_function(L: Plua_State): Integer; cdecl;
var count, i, upvalues : Integer;
begin
  count    := lua_gettop( L );
  upvalues := count;
  for i := 1 to count do
    if lua_isnoneornil( L, i )
      then Dec( upvalues )
      else luaL_checktype( L, i, LUA_TFUNCTION );

  if upvalues = 0 then Exit( 0 );

  lua_pushinteger( L, upvalues );
  for i := 1 to count do
    if not lua_isnoneornil( L, i ) then
      lua_pushvalue( L, i );

  if upvalues = 1 then Exit( 1 );
  lua_pushcclosure( L, @lua_core_create_seq_function_closure, upvalues + 1 );
  Result := 1;
end;

function lua_core_type_flags(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 3 ) <> LUA_TTABLE then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, flags expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  vlua_table_toset( L, 3 );
  Result := 1;
end;

function lua_core_type_nid( L : PLua_State ) : Integer; cdecl;
var iLua : TLua;
begin
  if lua_type( L, 3 ) =  LUA_TNUMBER  then Exit( 0 );
  if lua_type( L, 3 ) <> LUA_TSTRING then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, existing id expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  iLua := TLuaContext.FromState( L ).Lua;
  if not iLua.Defines.Exists( lua_tostring( L, 3 ) ) then
    luaL_error( L, 'LUA: "%s.%s" - id "%s" isn''t valid (yet?)!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_tolstring( L, 3, nil ) );
  lua_pushinteger( L, iLua.Defines[ lua_tostring( L, 3 ) ] );
  Result := 1;
end;

function lua_core_type_id( L : PLua_State ) : Integer; cdecl;
var iLua : TLua;
begin
  iLua := TLuaContext.FromState( L ).Lua;
  if lua_type( L, 3 ) <> LUA_TSTRING then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, existing id expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  if not iLua.Defines.Exists( lua_tostring( L, 3 ) ) then
    luaL_error( L, 'LUA: "%s.%s" - id "%s" isn''t valid (yet?)!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_tolstring( L, 3, nil ) );
  Result := 0;
end;

function lua_core_type_blueprint(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 3 ) <> LUA_TSTRING then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, blueprint id expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  if not blueprint_exists(L, 3) then
    luaL_error( L, 'LUA: "%s.%s" - blueprint "%s" isn''t valid (yet?)!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_tolstring( L, 3, nil ) );
  Result := 0;
end;

function lua_core_type_array_closure(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 3 ) <> LUA_TTABLE then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, ARRAY expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  lua_settop( L, 3 );
  lua_pushvalue( L, lua_upvalueindex(1) ); // push type index 4

  lua_pushvalue( L, 1 );
  lua_pushstring( L, '.' );
  lua_pushvalue( L, 2 );
  lua_concat( L, 3 ); // new ident index 5

  lua_pushnil(L);
  while lua_next( L, 3 ) <> 0 do
  begin
     // key (index -2), value (index -1)
     if lua_type( L, -2 ) <> LUA_TNUMBER then luaL_error( L, 'LUA: "%s.%s" - type mismatch, ARRAY expected, field found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ) );
     if lua_core_check_type_raw( L, 5, -2, -1, 4 ) then
     begin
       lua_pushvalue( L, -3 );  // Key
       lua_insert( L, -2 );
       lua_rawset( L, -3 ); // update Value
     end;
     lua_pop( L, 1 );
  end;
  Result := 0;
end;

function lua_core_type_array(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop( L ) <> 1 then luaL_error( L, 'Misuse of core.TARRAY type - usage is core.TARRAY( type )' );
  lua_pushcclosure( L, @lua_core_type_array_closure, 1 );
  Result := 1;
end;

function lua_core_type_map_closure(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 3 ) <> LUA_TTABLE then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, MAP expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  lua_settop( L, 3 );
  lua_pushvalue( L, lua_upvalueindex(1) ); // push key type index 4
  lua_pushvalue( L, lua_upvalueindex(2) ); // push value type index 5

  lua_pushvalue( L, 1 );
  lua_pushstring( L, '.' );
  lua_pushvalue( L, 2 );
  lua_concat( L, 3 ); // new ident index 6

  lua_pushnil(L);
  while lua_next( L, 3 ) <> 0 do
  begin
     // key (index -2), value (index -1)
     if lua_core_check_type_raw( L, 6, -2, -2, 4 ) then luaL_error( L, 'LUA: "%s.%s" - KEY type can''t be mutable!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ) );
     if lua_core_check_type_raw( L, 6, -2, -1, 5 ) then
     begin
       lua_pushvalue( L, -3 );  // Key
       lua_insert( L, -2 );
       lua_rawset( L, -3 ); // update Value
     end;
     lua_pop( L, 1 );
  end;
  Result := 0;
end;

function lua_core_type_map(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop( L ) <> 2 then luaL_error( L, 'Misuse of core.TMAP type - usage is core.TMAP( keytype, valuetype )' );
  lua_pushcclosure( L, @lua_core_type_map_closure, 2 );
  Result := 1;
end;

function lua_core_type_idin_closure(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 3 ) <> LUA_TSTRING then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, ID expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  lua_settop( L, 3 );
  lua_pushvalue( L, lua_upvalueindex(1) ); // push type index 4
  if lua_type( L, -1 ) = LUA_TSTRING then
    lua_rawget_global( L );
  if lua_type( L, -1 ) <> LUA_TTABLE then
    luaL_error( L, 'LUA: not a valid storage table in TIDIN!', lua_tolstring( L, lua_upvalueindex(1), nil ) );
  lua_pushvalue( L, 3 );
  lua_rawget( L, -2 );
  if lua_isnoneornil( L, -1 )  then
    luaL_error( L, 'LUA: "%s.%s" - valid ID expected!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ) );
  Result := 0;
end;

function lua_core_type_idin(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop( L ) <> 1 then luaL_error( L, 'Misuse of core.TIDIN type - usage is core.TIDIN( storage )' );
  lua_pushcclosure( L, @lua_core_type_idin_closure, 1 );
  Result := 1;
end;

function lua_core_to_object(L: Plua_State): Integer; cdecl;
var iObject : TObject;
begin
  iObject := vlua_toobject( L, 1 );
  vlua_pushobject( L, iObject );
  Exit( 1 );
end;

const lua_core_lib : array[0..31] of luaL_Reg = (
    ( name : 'TID';                      func : @lua_core_type_id),
    ( name : 'TNID';                     func : @lua_core_type_nid),
    ( name : 'TFLAGS';                   func : @lua_core_type_flags),
    ( name : 'TBLUEPRINT';               func : @lua_core_type_blueprint),
    ( name : 'TARRAY';                   func : @lua_core_type_array),
    ( name : 'TMAP';                     func : @lua_core_type_map),
    ( name : 'TIDIN';                    func : @lua_core_type_idin),
    ( name : 'log';                      func : @lua_core_log),
    ( name : 'warning';                  func : @lua_core_warning),
    ( name : 'iif';                      func : @lua_core_iif),
    ( name : 'register_blueprint';       func : @lua_core_register_blueprint),
    ( name : 'register_storage';         func : @lua_core_register_storage),
    ( name : 'create_constructor';       func : @lua_core_create_constructor),
    ( name : 'register_array_storage';   func : @lua_core_register_array_storage),
    ( name : 'create_array_constructor'; func : @lua_core_create_constructor),
    ( name : 'register';                 func : @lua_core_register),
    ( name : 'unregister';               func : @lua_core_unregister),
    ( name : 'array_register';           func : @lua_core_array_register),
    ( name : 'define';                   func : @lua_core_define),
    ( name : 'undefine';                 func : @lua_core_undefine),
    ( name : 'declare';                  func : @lua_core_declare),
    ( name : 'register_hook';            func : @lua_core_register_hook),
    ( name : 'make_id';                  func : @lua_core_make_id),
    ( name : 'set_rseed';                func : @lua_core_set_rseed),
    ( name : 'require';                  func : @lua_valkyrie_require),
    ( name : 'print';                    func : @lua_valkyrie_print),
    ( name : 'create_seq_function';      func : @lua_core_create_seq_function),
    ( name : 'to_object';                func : @lua_core_to_object),

    ( name : 'apply_blueprint';          func : @lua_core_apply_blueprint ),
    ( name : 'apply_blueprint_values';   func : @lua_core_apply_blueprint_values ),
    ( name : 'register_callback';        func : @lua_core_register_callback ),
    ( name : nil;              func : nil; )
);

{ TLuaClassInfo }

constructor TLuaClassInfo.Create ( const Proto, Storage : AnsiString ) ;
begin
  FProto := Proto;
  FStorage := Storage;
  FHookSet := [];
  FHookMax := 0;
end;

procedure TLuaClassInfo.RegisterHook ( aHookID : Byte; const aHookName : AnsiString ) ;
begin
  if aHookID >= High(FHooks) then SetLength( FHooks, Max(Max( 2*Length( FHooks ), 16 ),aHookID+1 ) );
  FHooks[ aHookID ] := aHookName;
  Include( FHookSet, aHookID );
  FHookMax := Max( FHookMax, aHookID );
end;

procedure TLuaClassInfo.RegisterHooks( const aHooks: THookSet; const aHookNames: array of AnsiString);
var iHook : Byte;
begin
  for iHook in aHooks do
    RegisterHook( iHook, aHookNames[ iHook ] );
end;

function TLuaClassInfo.GetHook ( HookID : Byte ) : AnsiString;
begin
  if HookID > High(FHooks) then Exit('');
  Exit( FHooks[ HookID ] );
end;

function TLuaClassInfo.GetHookID( HookName : AnsiString ) : Integer;
var b : Byte;
begin
  for b in FHookSet do
    if FHooks[b] = HookName then Exit(b);
  Exit( -1 );
end;

constructor TLua.Create( coverState : Plua_State = nil );
var i : Integer;
begin
  inherited Create;
  LoadLua;
  FCallDefVal  := NULL;
  FState       := TLuaState.Create( coverState );
  FRaw         := FState.NativeState;
  FContext     := TLuaContext.Create( Self );
  lua_pushlightuserdata( FRaw, @LuaContextKey );
  lua_pushlightuserdata( FRaw, FContext );
  lua_rawset( FRaw, LUA_REGISTRYINDEX );
  FModuleNames := TStringBoolMap.Create;
  FDataFiles   := TStringDataFileMap.Create;
  FRawModules  := TStringStringMap.Create;
  FDefines     := TIntMap.Create( HashMap_RaiseAll );
  FErrorFunc   := nil;
  FClassMap    := TLuaClassMap.Create();
  FStack.Init( FRaw );
  vlua_register( FRaw, 'print', @lua_valkyrie_print );
  vlua_register( FRaw, 'core', lua_core_lib );

  lua_getglobal( FRaw, 'core' );
  for i := Low( BlueprintTypes ) to High( BlueprintTypes ) do
  begin
    lua_pushinteger( FRaw, i );
    lua_setfield( FRaw, -2, PChar(BlueprintTypes[ i ]) );
  end;
  lua_newtable( FRaw );
  lua_setfield( FRaw, -2, 'blueprints');
  lua_newtable( FRaw );
  lua_setfield( FRaw, -2, 'callbacks');
  lua_pop( FRaw, 1 );
end;

destructor TLua.Destroy;
begin
  // A borrowed interpreter survives this system; remove its context reference.
  // An owned interpreter discards the registry when FState closes it below.
  if ( FContext <> nil ) and ( not FState.Owner ) then
  begin
    lua_pushlightuserdata( FRaw, @LuaContextKey );
    lua_pushnil( FRaw );
    lua_rawset( FRaw, LUA_REGISTRYINDEX );
  end;
  FreeAndNil( FModuleNames );
  FreeAndNil( FDataFiles );
  FreeAndNil( FRawModules );
  FreeAndNil( FClassMap );
  FreeAndNil( FDefines );
  FreeAndNil( FState );
  FreeAndNil( FContext );
  inherited Destroy;
end;

function TLua.RawDefined( const aValue : AnsiString ) : Boolean;
begin
  lua_pushstring( FRaw, PChar(aValue) );
  lua_rawget_global( FRaw );
  Result := not lua_isnil( FRaw, -1 );
  lua_pop( FRaw, 1 );
end;

function TLua.Defined(const Path: AnsiString): Boolean;
begin
  if not vlua_getpath( FRaw, Path ) then Exit( False );
  lua_pop( FRaw, 1 );
  Exit( True );
end;

function TLua.Defined(const Path: array of const): Boolean;
begin
  if not vlua_getpath( FRaw, Path ) then Exit( False );
  lua_pop( FRaw, 1 );
  Exit( True );
end;

function TLua.Tables(const Path: AnsiString): TLuaTablesEnumerator;
begin
  Tables.Create( FRaw, Path );
end;

function TLua.Tables(const Path: array of const): TLuaTablesEnumerator;
begin
  Tables.Create( FRaw, Path );
end;

function TLua.ITables(const Path: AnsiString): TLuaITablesEnumerator;
begin
  ITables.Create( FRaw, Path );
end;

function TLua.ITables(const Path: array of const): TLuaITablesEnumerator;
begin
  ITables.Create( FRaw, Path );
end;

function TLua.GetTable ( const Path : AnsiString ) : TLuaTable;
begin
  Exit( TLuaTable.Create( Raw, Path ) );
end;

function TLua.GetTable ( const Path : array of const ) : TLuaTable;
begin
  Exit( TLuaTable.Create( Raw, Path ) );
end;

function TLua.GetTableSize ( const Path : AnsiString ) : DWord;
begin
  GetTableSize := 0;
  if not vlua_getpath( FRaw, Path ) then Exit(0);
  if lua_istable( FRaw, -1 ) then
    GetTableSize := lua_objlen( FRaw, -1 );
  lua_pop( FRaw, 1 );
end;

function TLua.GetTableSize ( const Path : array of const ) : DWord;
begin
  GetTableSize := 0;
  if not vlua_getpath( FRaw, Path ) then Exit(0);
  if lua_istable( FRaw, -1 ) then
    GetTableSize := lua_objlen( FRaw, -1 );
  lua_pop( FRaw, 1 );
end;

function TLua.Get(const Path: AnsiString): Variant;
begin
  if not vlua_getpath( FRaw, Path ) then raise ELuaException.Create('Get('+Path+') failed!');
  Get := vlua_tovariant( FRaw, -1 );
  lua_pop( FRaw, 1 );
end;

function TLua.Get(const Path: array of const): Variant;
begin
  if not vlua_getpath( FRaw, Path ) then raise ELuaException.Create('Get('+PathToString( Path )+') failed!');
  Get := vlua_tovariant( FRaw, -1 );
  lua_pop( FRaw, 1 );
end;

function TLua.Get(const Path: AnsiString; const DefVal: Variant
  ): Variant;
begin
  if not vlua_getpath( FRaw, Path ) then Exit( DefVal );
  Get := vlua_tovariant( FRaw, -1, DefVal );
  lua_pop( FRaw, 1 );
end;

function TLua.Get(const Path: array of const; const DefVal: Variant
  ): Variant;
begin
  if not vlua_getpath( FRaw, Path ) then Exit( DefVal );
  Get := vlua_tovariant( FRaw, -1, DefVal );
  lua_pop( FRaw, 1 );
end;

procedure TLua.SetValue ( const Path : AnsiString; const Value : Variant ) ;
begin
  GetPath( Path );
  vlua_pushvariant( FRaw, Value );
  lua_rawset( FRaw, -3 );
  lua_pop( FRaw, 1 );
end;

procedure TLua.SetValue ( const Path : array of const; const Value : Variant ) ;
begin
  GetPath( Path );
  vlua_pushvariant( FRaw, Value );
  lua_rawset( FRaw, -3 );
  lua_pop( FRaw, 1 );
end;

procedure TLua.SetValue ( const Path : AnsiString; aObject : TObject ) ;
begin
  GetPath( Path );
  vlua_pushobject( FRaw, aObject );
  lua_rawset( FRaw, -3 );
  lua_pop( FRaw, 1 );
end;

procedure TLua.SetValue ( const Path : array of const; aObject : TObject ) ;
begin
  GetPath( Path );
  vlua_pushobject( FRaw, aObject );
  lua_rawset( FRaw, -3 );
  lua_pop( FRaw, 1 );
end;

function TLua.Call(const Path: array of const; const Args: array of const): Variant;
begin
  if not vlua_getpath( FRaw, Path ) then raise ELuaException.Create('Call('+PathToString( Path )+') not found!');
  try
    if not lua_isfunction( FRaw, -1 ) then raise ELuaException.Create('Call('+PathToString( Path )+') not a function!');
    vlua_pusharray( FRaw, Args );
    if lua_pcall( FRaw, High( Args ) + 1, 1, 0 ) <> 0 then  raise ELuaException.Create( 'Call('+PathToString( Path )+') Lua error : '+lua_tostring( FRaw, -1) );
    Call := vlua_tovariant( FRaw, -1, FCallDefVal );
  finally
    lua_pop( FRaw, 1 );
  end;
end;

function TLua.Call(const Path: AnsiString; const Args: array of const): Variant;
begin
  if not vlua_getpath( FRaw, Path ) then raise ELuaException.Create('Call('+Path+') not found!');
  try
    if not lua_isfunction( FRaw, -1 ) then raise ELuaException.Create('Call('+Path+') not a function!');
    vlua_pusharray( FRaw, Args );
    if lua_pcall( FRaw, High( Args ) + 1, 1, 0 ) <> 0 then  raise ELuaException.Create( 'Call('+Path+') Lua error : '+lua_tostring( FRaw, -1) );
    Call := vlua_tovariant( FRaw, -1, FCallDefVal );
  finally
    lua_pop( FRaw, 1 );
  end;
end;

function TLua.GetProtoTable ( aObj : TObject ) : TLuaTable;
begin
  Exit( TLuaTable.Create( Raw, [ FClassMap[ aObj.ClassName ].Storage, (aObj as ILuaReferencedObject).GetID ] ) );

end;

function TLua.RunHook(Obj: ILuaReferencedObject; HookName: AnsiString; const Params: array of const): Variant;
begin
  FStack.Init( Raw );
  RunHook := FStack.RunHook( Obj, HookName, Params );
end;

function TLua.ProtectedCall(const Path: array of const; const Args: array of const): Variant;
begin
  try
    Exit( Call( Path, Args ) );
  except on e : Exception do
  begin
    ErrorLogOpen('ERROR','Lua call '+DebugToString(@Path[High(Path)])+' caught '+e.ClassName+'!');
    ErrorLogWriteln('Call path     : '+PathToString( Path ));
    ErrorLogWriteln('Call params   : '+DebugToString( Args ));
    ErrorLogWriteln('Error message : '+e.Message);
    ErrorLogClose;
    ProtectedCall := False;
    OnError( PathToString( Path ) + ' -- ' + e.Message );
  end;
  end;
end;

function TLua.ProtectedCall(const Path: AnsiString; const Args: array of const): Variant;
begin
  try
    Exit( Call( Path, Args ) );
  except on e : Exception do
  begin
    ErrorLogOpen('ERROR','Lua call '+Path+' caught '+e.ClassName+'!');
    ErrorLogWriteln('Call path     : '+Path );
    ErrorLogWriteln('Call params   : '+DebugToString( Args ));
    ErrorLogWriteln('Error message : '+e.Message);
    ErrorLogClose;
    ProtectedCall := False;
    OnError( Path + ' -- ' + e.Message );
  end;
  end;
end;

function TLua.ProtectedRunHook(Obj: ILuaReferencedObject; HookName: AnsiString; const Params: array of const): Variant;
begin
  try
    ProtectedRunHook := FStack.RunHook( Obj, HookName, Params );
  except
    on e : Exception do
    begin
      ErrorLogOpen('ERROR','Lua hook '+HookName+' caught '+e.ClassName+'!');
      ErrorLogWriteln('Call path     : '+Obj.GetProtoTable+'['+Obj.GetID+'].'+HookName );
      ErrorLogWriteln('Call params   : '+DebugToString( Params ));
      ErrorLogWriteln('Error message : '+e.Message);
      ErrorLogClose;
      ProtectedRunHook := False;
      OnError( Obj.GetProtoTable+'['+Obj.GetID+'].'+HookName + ' -- ' + e.Message );
    end;
  end;
end;

procedure TLua.Register(const libname: AnsiString; const lr: PluaL_Reg);
begin
  vlua_register( FRaw, libname, lr );
end;

procedure TLua.RegisterSubTable ( const aTableName, aSubTable : AnsiString ) ;
begin
  lua_getglobal( FRaw, aTableName );
    lua_pushstring( FRaw, PChar(aSubTable) );
    lua_newtable( FRaw );
    lua_rawset( FRaw, -3 );
  lua_pop( FRaw, 1 );
end;

procedure TLua.RegisterMetaTable ( const aTableName : AnsiString;  const aIndexFunc, aNewIndexFunc : lua_CFunction ) ;
begin
  lua_getglobal( FRaw, aTableName );
  if lua_isnil( FRaw, -1 ) then
  begin
    lua_pop( FRaw, 1 );
    lua_createtable( FRaw, 0, 0 );
    lua_setglobal( FRaw, aTableName );
    lua_getglobal( FRaw, aTableName );
  end;
    lua_createtable( FRaw, 0, 2 );
      lua_pushcfunction( FRaw, aIndexFunc );
      lua_setfield( FRaw, -2, '__index' );
      lua_pushcfunction( FRaw,  aNewIndexFunc );
      lua_setfield( FRaw, -2, '__newindex' );
    lua_setmetatable( FRaw, -2 );
  lua_pop( FRaw, 1 );
end;

procedure TLua.RegisterMetaTable ( const aTableName, aSubTable : AnsiString; const aIndexFunc, aNewIndexFunc : lua_CFunction ) ;
begin
  lua_getglobal( FRaw, aTableName );
    lua_pushstring( FRaw, PChar(aSubTable) );
    lua_newtable( FRaw );
      lua_createtable( FRaw, 0, 2 );
        lua_pushcfunction( FRaw, aIndexFunc );
        lua_setfield( FRaw, -2, '__index' );
        lua_pushcfunction( FRaw, aNewIndexFunc );
        lua_setfield( FRaw, -2, '__newindex' );
      lua_setmetatable( FRaw, -2 );
    lua_rawset( FRaw, -3 );
  lua_pop( FRaw, 1 );
end;

procedure TLua.LoadFile( const FileName: AnsiString );
begin
  if luaL_dofile( FRaw, PChar(FileName) ) <> 0 then
    raise ELuaException.Create( lua_tostring(FRaw,-1) );
end;

procedure TLua.LoadStream( IST: TStream; StreamName: AnsiString; Size: DWord );
begin
  Log('Reading "'+StreamName+'" size ('+IntToStr(Size)+'bytes) ('+IntToStr(IST.Position)+'-'+IntToStr(IST.Position+Size)+')');
  if vlua_loadstream( FRaw, IST, Size, StreamName ) <> 0 then
    begin
      OnError(StreamName+': '+lua_tostring(FRaw,-1));
      lua_pop(FRaw,1);
      Exit;
    end;
  FreeAndNil( ISt );
  if lua_pcall(FRaw, 0, 0, 0)  <> 0 then
  begin
    OnError(StreamName+': '+lua_tostring(FRaw,-1));
    lua_pop(FRaw,1);
  end;

  Log('Loaded "'+StreamName+'" ('+IntToStr(Size)+'bytes)');
end;

procedure TLua.LoadStream(DF: TVDataFile; const StreamName: AnsiString);
var Stream : TStream;
    Size   : Int64;
begin
  Stream := DF.GetFile( StreamName );
  Size   := DF.GetFileSize( StreamName );
  LoadStream( Stream, StreamName, Size );
end;


procedure TLua.LoadStream(DF: TVDataFile; const DirName, FileName: AnsiString);
var Stream : TStream;
    Size   : Int64;
begin
  Stream := DF.GetFile( FileName, DirName );
  Size   := DF.GetFileSize( FileName, DirName );
  LoadStream( Stream, FileName, Size );
end;

procedure TLua.OnError(const Message: AnsiString);
begin
  if Assigned( FErrorFunc ) then FErrorFunc( Message );
end;

procedure TLua.RegisterModule( const ModuleName: AnsiString; DF: TVDataFile );
begin
  FDataFiles[ ModuleName ] := DF;
  FState.Register('require', @lua_valkyrie_require );
end;

procedure TLua.RegisterModule( const ModuleName, ModulePath: AnsiString);
begin
  FRawModules[ ModuleName ] := ModulePath;
  FState.Register('require', @lua_valkyrie_require );
end;

procedure TLua.RegisterType(AClass: TClass; const ProtoName, StorageName: AnsiString);
begin
  FClassMap[ AClass.ClassName ] := TLuaClassInfo.Create( ProtoName, StorageName );
end;

function TLua.GetClassInfo ( AClass : TClass ) : TLuaClassInfo;
begin
  GetClassInfo := FClassMap[ AClass.ClassName ];
  Assert( GetClassInfo <> nil );
end;

function TLua.GetProtoTable(AClass: TClass): AnsiString;
begin
  Exit( FClassMap[ AClass.ClassName ].Proto );
end;

function TLua.GetStorageTable(AClass: TClass): AnsiString;
begin
  Exit( FClassMap[ AClass.ClassName ].Storage );
end;

function TLua.RegisterObject(Obj: TObject; aClassName : AnsiString = '' ): Integer;
begin
  if aClassName = '' then aClassName := Obj.ClassName;
  lua_getglobal( FRaw, FClassMap[ aClassName ].Proto );
  if lua_isnil( FRaw, -1 ) then raise ELuaException.Create( Obj.ClassName + ' type not registered!' );
  DeepPointerCopy( -1, Obj );
  RegisterObject := luaL_ref( FRaw, LUA_REGISTRYINDEX );
  lua_pop( FRaw, 1);
end;

procedure TLua.UnRegisterObject(Obj: ILuaReferencedObject);
begin
  lua_rawgeti( FRaw, LUA_REGISTRYINDEX, Obj.GetLuaIndex );
  lua_pushstring( FRaw, '__ptr' );
  lua_pushboolean( FRaw, False );
  lua_rawset( FRaw, -3 );
  lua_pop( FRaw, 1 );
  luaL_unref( FRaw, LUA_REGISTRYINDEX, Obj.GetLuaIndex );
end;

function TLua.GetMemoryKB : DWord;
begin
  Exit( lua_gc( FRaw, LUA_GCCOUNT, 0 ) );
end;

function TLua.GetMemoryB : DWord;
begin
  Exit( 1024*lua_gc( FRaw, LUA_GCCOUNT, 0 ) + lua_gc( FRaw, LUA_GCCOUNTB, 0 ) );
end;

procedure TLua.CollectGarbage;
begin
  lua_gc( FRaw, LUA_GCCOLLECT, 0 );
end;

procedure TLua.SetPrintFunction ( aPrintFunc : TLuaPrintFunc ) ;
begin
  FPrintFunc := aPrintFunc;
end;

procedure TLua.Print ( const aText : AnsiString ) ;
begin
  if Assigned( FPrintFunc ) then
    FPrintFunc( aText );
end;

procedure TLua.ConsoleExecute ( const aCode : AnsiString ) ;
var iError : AnsiString;
    iCode  : Integer;
    iStack : Integer;
    cmd    : AnsiString;
begin
  cmd := Trim(aCode);
  if length(cmd) = 0 then Exit;
  iStack := lua_gettop(FRaw);
  Print('({B'+IntToStr(iStack)+'})> '+cmd);

  if cmd[1] = '=' then
  begin
    Delete(cmd,1,1);
    cmd := 'return '+cmd;
  end;

  iCode := luaL_loadstring(FRaw, PChar(cmd));
  if iCode = 0 then iCode := lua_pcall(FRaw, 0, LUA_MULTRET, 0);
  if iCode <> 0 then
  begin
    iError := lua_tostring(FRaw,-1);
    Print('{RError: }'+iError);
    lua_pop(FRaw,1);
    Exit;
  end;

  if lua_gettop(FRaw) > iStack then
  for iCode := iStack+1 to lua_gettop(FRaw) do
    PrintValue( FRaw, iCode );
  lua_settop(FRaw,iStack);
end;

procedure TLua.SetErrorFunc ( aErrorFunc : TLuaErrorFunc ) ;
begin
  FErrorFunc     := aErrorFunc;
  FState.ErrorFunc := aErrorFunc;
end;

procedure TLua.TableToStream ( const aPath : AnsiString; aStream : TStream ) ;
begin
  if (not vlua_getpath( FRaw, aPath )) or (not lua_istable( FRaw, -1 )) then raise ELuaException.Create('TableToStream('+aPath+') not found!');
  vlua_tabletostream( FRaw, -1, aStream );
  lua_pop( FRaw, 1 );
end;

procedure TLua.TableFromStream ( const aPath : AnsiString; aStream : TStream ) ;
begin
  if not vlua_getpath( FRaw, aPath ) then raise ELuaException.Create('TableFromStream('+aPath+') path not setup!');
  vlua_tablefromstream( FRaw, -1, aStream );
  lua_pop( FRaw, 1 );
end;

procedure TLua.GetPath ( const Path : AnsiString );
var RP   : Word;
begin
  RP := RPos( '.', Path );
  if RP < 1 then
  begin
    lua_pushglobaltable( FRaw );
    lua_pushansistring( FRaw, Path );
  end
  else
  begin
    if not vlua_getpath( FRaw, LeftStr( Path, RP-1 ) ) then raise ELuaException.Create('Get('+Path+') failed!');
    lua_pushansistring( FRaw, Copy( Path, RP+1, Length( Path ) - RP ) );
  end;
end;

procedure TLua.GetPath ( const Path : array of const );
begin
  Assert( High( Path ) >= 0 );
  if High( Path ) = 0 then
  begin
    lua_pushglobaltable( FRaw );
    vlua_pushvarrec( FRaw, @Path[0] );
  end
  else
  begin
    if not vlua_getpath( FRaw, Path, 0, High(Path) - 1 ) then raise ELuaException.Create('Get('+PathToString( Path )+') failed!');
    vlua_pushvarrec( FRaw, @Path[High( Path )] );
  end;
end;

function TLua.PathToString(const Path: array of const): AnsiString;
var i : Integer;
begin
try
  If High(Path) < 0 then
  begin
    Exit('<empty>');
  end;
  PathToString := '';
  for i:=0 to High(Path) do
  begin
    if i <> 0 then PathToString += '.';
    PathToString += DebugToString(@(path[i]));
  end;
except on e : Exception do
  PathToString := 'exception on PathToString'
end;
end;

procedure TLua.DeepPointerCopy(Index: Integer; Obj
: Pointer );
var HasFunctions : Boolean;
    HasMetatable : Boolean;
begin
  index := lua_absindex( FRaw, index );
  lua_newtable( FRaw );
  lua_pushnil( FRaw );
  HasFunctions := false;
  HasMetatable := false;

  while lua_next( FRaw, index ) <> 0 do
  begin
    if lua_isfunction( FRaw, -1 ) then HasFunctions := true
    else if lua_istable( FRaw, -1 ) then
    begin
      DeepPointerCopy( -1, Obj );
      lua_insert( FRaw, -2 );
      lua_pop( FRaw, 1 );
    end;
    lua_pushvalue( FRaw, -2 );
    lua_insert( FRaw, -2 );
    lua_settable( FRaw, -4 );
  end;

  if lua_getmetatable( FRaw, -2 ) then
  begin
    lua_setmetatable( FRaw, -2 );
    HasMetatable := true;
  end;

  if HasFunctions or HasMetatable then
  begin
    lua_pushstring( FRaw, '__ptr' );
    lua_pushlightuserdata( FRaw, Obj );
    lua_rawset( FRaw, -3 );
  end;
end;


end.
