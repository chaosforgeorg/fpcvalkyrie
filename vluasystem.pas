{$INCLUDE valkyrie.inc}
// @abstract(LuaSystem class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @cvs($Author: chaos-dev $)
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
//    -- TNode should hold it's lua state pointer!
//    -- Decouple TNode from LuaSystem
//    -- All lua-based creation should be based on properties!
//    -- use overrides to handle weird properties

unit vluasystem;
interface
uses classes, vlualibrary, vutil, vdebug, vsystem, vlua, vluastate, vluatype, vdf, vgenerics, vluatable;

type
   ELuaException = vlualibrary.ELuaException;
   PLua_State    = vlualibrary.PLua_State;
   PluaL_Reg     = vlualibrary.PluaL_Reg;
   LuaL_Reg      = vlualibrary.luaL_Reg;
   TLuaTable     = vluatable.TLuaTable;
   THookSet      = set of Byte;
   TLuaSystemErrorFunc = procedure( const Message : AnsiString ) of object;
   TLuaSystemPrintFunc = procedure( const Text : AnsiString ) of object;

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

type TLuaClassMap       = specialize TGHashMap<TLuaClassInfo>;
     TStringBoolMap     = specialize TGHashMap<Boolean>;
     TStringDataFileMap = specialize TGHashMap<TVDataFile>;
     TStringStringMap   = specialize TGHashMap<AnsiString>;
     TIntMap            = specialize TGHashMap<Integer>;

type

{ TLuaSystem }

 TLuaSystem = class(TSystem)
    // Registers system execution.
    constructor Create( coverState : Plua_State = nil ); reintroduce;
    // Closes system execution.
    destructor Destroy; override;
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
    // TLuaSystem handles it. Function should be overriden to react on
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
    procedure SetPrintFunction( aPrintFunc : TLuaSystemPrintFunc );
    // Print if assigned
    procedure Print( const aText : AnsiString );
    // Execute and print results
    procedure ConsoleExecute( const aCode : AnsiString );
    //
    procedure SetErrorFunc( aErrorFunc : TLuaSystemErrorFunc );
    // Streaming support
    procedure TableToStream( const aPath : AnsiString; aStream : TStream );
    // Streaming support
    procedure TableFromStream( const aPath : AnsiString; aStream : TStream );
  protected
    FLuaState     : TLuaState;
    FState        : PLua_State;
    FLua          : TLua;
    FErrorFunc    : TLuaSystemErrorFunc;
    FPrintFunc    : TLuaSystemPrintFunc;
    FModuleNames  : TStringBoolMap;
    FDataFiles    : TStringDataFileMap;
    FRawModules   : TStringStringMap;
    FClassMap     : TLuaClassMap;
    FCallDefVal   : Variant;
    FDefines      : TIntMap;
  public
    property CallDefaultResult : Variant     read FCallDefVal write FCallDefVal;
    property Raw : PLua_State                read FState;
    property State : TLuaState               read FLuaState;
    property ErrorFunc : TLuaSystemErrorFunc write SetErrorFunc;
    property ModuleNames : TStringBoolMap    read FModuleNames;
    property Defines : TIntMap               read FDefines;

  private
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

const LuaSystem : TLuaSystem = nil;

implementation

uses variants, sysutils, strutils, math, vluaext;

const BlueprintTypes : array[-1..8] of PChar = ( 'TANY', 'TNIL', 'TBOOL', 'TLUSER', 'TNUMBER', 'TSTRING', 'TTABLE', 'TFUNC', 'TUSER', 'TTHREAD' );

function print_value(L: Plua_State; index : Integer; Indent : Word = 0; Prefix : AnsiString = '') : Word;
var Lines : Byte;
begin
  index := lua_absindex(L,index);
  Prefix := StringOfChar(' ',Indent)+Prefix;
  case lua_type(L,index) of
    LUA_TNIL           : LuaSystem.Print(Prefix+'@Bnil');
    LUA_TBOOLEAN       : if lua_toboolean(L,index) then LuaSystem.Print(Prefix+'@Btrue') else LuaSystem.Print(Prefix+'@Bfalse');
    LUA_TLIGHTUSERDATA : LuaSystem.Print(Prefix+'@blightuserdata(@B0x'+hexstr(lua_touserdata(L,index))+'@b)');
    LUA_TNUMBER        : LuaSystem.Print(Prefix+'@L'+lua_tostring(L,index));
    LUA_TSTRING        : LuaSystem.Print(Prefix+'"'+lua_tostring(L,index)+'"');
    LUA_TFUNCTION      : LuaSystem.Print(Prefix+'@yfunction');
    LUA_TUSERDATA      : LuaSystem.Print(Prefix+'@yuserdata');
    LUA_TTHREAD        : LuaSystem.Print(Prefix+'@ythread');
    LUA_TTABLE         :
      begin
        LuaSystem.Print(Prefix+'@ytable@> = {');
        Indent += 2;
        Lines := 2;
        lua_pushnil(L);
        while lua_next(L, index) <> 0 do
        begin
          // key (index -2), 'value' (index -1)
          if lua_isnumber( L, -2 ) then
            Lines += print_value( L, -1, Indent, IntToStr(lua_tointeger( L, -2 ))+' = ')
          else
            Lines += print_value( L, -1, Indent, lua_tostring( L, -2 )+' = ');
          // remove value, keep key
          lua_pop(L, 1);
          if Lines > 8 then
          begin
            LuaSystem.Print(StringOfChar(' ',Indent)+'...');
            lua_pop(L, 1);
            break;
          end;
        end;
        if Lines <= 8 then LuaSystem.Print(StringOfChar(' ',Indent-2)+'}');
        Exit(Lines);
      end;
  end;
  Exit(1);
end;

function lua_valkyrie_print( L: Plua_State ) : Integer; cdecl;
var n : Integer;
begin
  if Assigned( LuaSystem.FPrintFunc ) then
  begin
    n := lua_gettop(L);
    if n <= 0 then Exit(0);
    for n := 1 to lua_gettop(L) do
      print_value(L,n);
  end;
  Result := 0;
end;

{ TLuaSystem }

function lua_valkyrie_require( L: Plua_State ) : Integer; cdecl;
var Arg      : AnsiString;
    Module   : AnsiString;
    Path     : AnsiString;
    FileName : AnsiString;
begin
  Log('LuaRequire, entering...');
  if lua_gettop(L) <> 1 then LuaSystem.OnError('Require has wrong amount of parameters!');
  Arg := lua_tostring( L, 1 );
  Log('LuaRequire("'+Arg+'")');

  if LuaSystem.FModuleNames.Exists(Arg) then Exit(0);

  Module := ExtractDelimited( 1, Arg, [':'] );
  Path := ExtractFilePath( Arg );
  if Module <> '' then
    Delete( Path, 1, Length( Module ) + 1 );

  if (Length(Path) > 0) and (Path[Length(Path)] = '/') then Delete(Path,Length(Path),1);
  FileName := ExtractFileName( Arg ) + '.lua';

  if Pos(':', FileName) > 0 then
    Delete( FileName, 1, Pos(':', FileName) );

  Log('LuaRequire( Module "'+Module+'", Path "'+Path+'", FileName "'+FileName+'")');

  if not LuaSystem.FRawModules.Exists(Module) then
  begin
    if not LuaSystem.FDataFiles.Exists(Module) then
      raise ELuaException.Create('require : Module "'+Module+'" not found!');
    LuaSystem.LoadStream( LuaSystem.FDataFiles[Module], Path, FileName );
  end
  else
  begin
    if Path <> '' then
      Path := LuaSystem.FRawModules[ Module ] + Path + DirectorySeparator + FileName
    else
      Path := LuaSystem.FRawModules[ Module ] + FileName;
    if not FileExists(Path) then
      raise ELuaException.Create('require : File "'+Path+'" not found!');
    LuaSystem.LoadFile( Path );
  end;

  LuaSystem.FModuleNames[ Arg ] := True;
  Exit( 0 );
end;

function lua_core_log(L: Plua_State): Integer; cdecl;
var State : TLuaState;
begin
  State.Init( L );
  Log( State.ToString(1) );
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
var State  : TLuaState;
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

function lua_core_register(L: Plua_State): Integer; cdecl; forward;

function lua_core_create_constructor_impl(L: Plua_State): Integer; cdecl;
var ident : ansistring;
begin
  luaL_checktype( L, 1, LUA_TTABLE );

  lua_pushvalue( L, lua_upvalueindex( 1 ) ); // id
  ident := vlua_tostring( L, -1 );
  if LuaSystem.Defines.Exists( ident ) then
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
    ident := vlua_tostring( L, -1 ) + '[' + ident + ']';
    lua_pop( L, 1 );

    lua_pushcfunction( L, @lua_core_apply_blueprint );
    lua_pushvalue( L, 1 );
    lua_pushvalue( L, -3 );
    lua_pushstring( L, PChar(ident) );
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

function lua_core_register(L: Plua_State): Integer; cdecl;
var iName,iID : AnsiString;
begin
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
  LuaSystem.FDefines[ iID ] := lua_tointeger( L, 3 );

  // return id
  lua_pushstring( L, PChar(iID) );
  Result := 1;
end;

function lua_core_unregister(L: Plua_State): Integer; cdecl;
var id : integer;
    s  : ansistring;
begin
  luaL_checktype( L, 1, LUA_TTABLE );

  if lua_type( L, 2 ) = LUA_TSTRING then
  begin
    s  := vlua_tostring(L,2);
    id := LuaSystem.Defines[ s ];
    LuaSystem.Defines.Remove( s );
    lua_pushvalue( L, 2 );
    lua_pushnil( L );
    lua_rawset( L, 1 );
    lua_pushinteger( L, id );
    lua_pushnil( L );
    lua_rawset( L, 1 );
    Exit(0);
  end;

  lua_pushnil(L);
  while lua_next( L, 1 ) <> 0 do
  begin
    if lua_type( L, -2 ) = LUA_TSTRING then
      LuaSystem.Defines.Remove(vlua_tostring(L,-2));
    lua_pushvalue( L, -2 );
    lua_pushnil( L );
    lua_rawset( L, 1 );
    lua_pop(L, 1);
  end;
  Result := 0;
end;

function lua_core_define(L: Plua_State): Integer; cdecl;
var State : TLuaState;
begin
  State.Init( L );
  LuaSystem.FDefines[ State.ToString(1) ] := State.ToInteger(2);
  Result := 0;
end;

function lua_core_undefine(L: Plua_State): Integer; cdecl;
var State : TLuaState;
begin
  State.Init(L);
  LuaSystem.Defines.Remove(State.ToString(1));
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

function lua_core_set_rseed(L: Plua_State): Integer; cdecl;
begin
  if lua_gettop(L) < 1 then Exit(0);
  RandSeed := lua_tointeger(L,1);
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

function lua_core_type_nid(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 3 ) =  LUA_TNUMBER  then Exit( 0 );
  if lua_type( L, 3 ) <> LUA_TSTRING then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, existing id expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  if not LuaSystem.Defines.Exists( lua_tostring( L, 3 ) ) then
    luaL_error( L, 'LUA: "%s.%s" - id "%s" isn''t valid (yet?)!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_tolstring( L, 3, nil ) );
  lua_pushinteger( L, LuaSystem.Defines[ lua_tostring( L, 3 ) ] );
  Result := 1;
end;

function lua_core_type_id(L: Plua_State): Integer; cdecl;
begin
  if lua_type( L, 3 ) <> LUA_TSTRING then
    luaL_error( L, 'LUA: "%s.%s" - type mismatch, existing id expected, %s found!', lua_tolstring( L, 1, nil ), lua_tolstring( L, 2, nil ), lua_typename( L, lua_type( L, 3 ) ) );
  if not LuaSystem.Defines.Exists( lua_tostring( L, 3 ) ) then
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

const lua_core_lib : array[0..28] of luaL_Reg = (
    ( name : 'TID';                      func : @lua_core_type_id),
    ( name : 'TNID';                     func : @lua_core_type_nid),
    ( name : 'TFLAGS';                   func : @lua_core_type_flags),
    ( name : 'TBLUEPRINT';               func : @lua_core_type_blueprint),
    ( name : 'TARRAY';                   func : @lua_core_type_array),
    ( name : 'TMAP';                     func : @lua_core_type_map),
    ( name : 'TIDIN';                    func : @lua_core_type_idin),
    ( name : 'log';                      func : @lua_core_log),
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

    ( name : 'apply_blueprint';          func : @lua_core_apply_blueprint ),
    ( name : 'apply_blueprint_values';   func : @lua_core_apply_blueprint_values ),
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
  if aHookID > High(FHooks) then SetLength( FHooks, Max(Max( 2*Length( FHooks ), 16 ),aHookID ) );
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

constructor TLuaSystem.Create( coverState : Plua_State = nil );
var i : Integer;
begin
  inherited Create;
  LoadLua;
  FCallDefVal  := NULL;
  FLua         := TLua.Create( coverState );
  FState       := FLua.NativeState;
  FModuleNames := TStringBoolMap.Create;
  FDataFiles   := TStringDataFileMap.Create;
  FRawModules  := TStringStringMap.Create;
  FDefines     := TIntMap.Create( HashMap_RaiseAll );
  FErrorFunc   := nil;
  FClassMap    := TLuaClassMap.Create();
  FLuaState.Init( FState );
  vlua_register( FState, 'print', @lua_valkyrie_print );
  vlua_register( FState, 'core', lua_core_lib );

  lua_getglobal( FState, 'core' );
  for i := Low( BlueprintTypes ) to High( BlueprintTypes ) do
  begin
    lua_pushinteger( FState, i );
    lua_setfield( FState, -2, PChar(BlueprintTypes[ i ]) );
  end;
  lua_newtable( FState );
  lua_setfield( FState, -2, 'blueprints');
  lua_pop( FState, 1 );
end;

destructor TLuaSystem.Destroy;
begin
  FreeAndNil( FModuleNames );
  FreeAndNil( FDataFiles );
  FreeAndNil( FRawModules );
  FreeAndNil( FClassMap );
  FreeAndNil( FDefines );
  inherited Destroy;
  LuaSystem := nil;
end;

function TLuaSystem.Defined(const Path: AnsiString): Boolean;
begin
  if not vlua_getpath( FState, Path ) then Exit( False );
  lua_pop( FState, 1 );
  Exit( True );
end;

function TLuaSystem.Defined(const Path: array of const): Boolean;
begin
  if not vlua_getpath( FState, Path ) then Exit( False );
  lua_pop( FState, 1 );
  Exit( True );
end;

function TLuaSystem.Tables(const Path: AnsiString): TLuaTablesEnumerator;
begin
  Tables.Create( FState, Path );
end;

function TLuaSystem.Tables(const Path: array of const): TLuaTablesEnumerator;
begin
  Tables.Create( FState, Path );
end;

function TLuaSystem.ITables(const Path: AnsiString): TLuaITablesEnumerator;
begin
  ITables.Create( FState, Path );
end;

function TLuaSystem.ITables(const Path: array of const): TLuaITablesEnumerator;
begin
  ITables.Create( FState, Path );
end;

function TLuaSystem.GetTable ( const Path : AnsiString ) : TLuaTable;
begin
  Exit( TLuaTable.Create( Raw, Path ) );
end;

function TLuaSystem.GetTable ( const Path : array of const ) : TLuaTable;
begin
  Exit( TLuaTable.Create( Raw, Path ) );
end;

function TLuaSystem.GetTableSize ( const Path : AnsiString ) : DWord;
begin
  GetTableSize := 0;
  if not vlua_getpath( FState, Path ) then Exit(0);
  if lua_istable( FState, -1 ) then
    GetTableSize := lua_objlen( FState, -1 );
  lua_pop( FState, 1 );
end;

function TLuaSystem.GetTableSize ( const Path : array of const ) : DWord;
begin
  GetTableSize := 0;
  if not vlua_getpath( FState, Path ) then Exit(0);
  if lua_istable( FState, -1 ) then
    GetTableSize := lua_objlen( FState, -1 );
  lua_pop( FState, 1 );
end;

function TLuaSystem.Get(const Path: AnsiString): Variant;
begin
  if not vlua_getpath( FState, Path ) then raise ELuaException.Create('Get('+Path+') failed!');
  Get := vlua_tovariant( FState, -1 );
  lua_pop( FState, 1 );
end;

function TLuaSystem.Get(const Path: array of const): Variant;
begin
  if not vlua_getpath( FState, Path ) then raise ELuaException.Create('Get('+PathToString( Path )+') failed!');
  Get := vlua_tovariant( FState, -1 );
  lua_pop( FState, 1 );
end;

function TLuaSystem.Get(const Path: AnsiString; const DefVal: Variant
  ): Variant;
begin
  if not vlua_getpath( FState, Path ) then Exit( DefVal );
  Get := vlua_tovariant( FState, -1, DefVal );
  lua_pop( FState, 1 );
end;

function TLuaSystem.Get(const Path: array of const; const DefVal: Variant
  ): Variant;
begin
  if not vlua_getpath( FState, Path ) then Exit( DefVal );
  Get := vlua_tovariant( FState, -1, DefVal );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.SetValue ( const Path : AnsiString; const Value : Variant ) ;
begin
  GetPath( Path );
  vlua_pushvariant( FState, Value );
  lua_rawset( FState, -3 );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.SetValue ( const Path : array of const; const Value : Variant ) ;
begin
  GetPath( Path );
  vlua_pushvariant( FState, Value );
  lua_rawset( FState, -3 );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.SetValue ( const Path : AnsiString; aObject : TObject ) ;
begin
  GetPath( Path );
  vlua_pushobject( FState, aObject );
  lua_rawset( FState, -3 );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.SetValue ( const Path : array of const; aObject : TObject ) ;
begin
  GetPath( Path );
  vlua_pushobject( FState, aObject );
  lua_rawset( FState, -3 );
  lua_pop( FState, 1 );
end;

function TLuaSystem.Call(const Path: array of const; const Args: array of const): Variant;
begin
  if not vlua_getpath( FState, Path ) then raise ELuaException.Create('Call('+PathToString( Path )+') not found!');
  try
    if not lua_isfunction( FState, -1 ) then raise ELuaException.Create('Call('+PathToString( Path )+') not a function!');
    vlua_pusharray( FState, Args );
    if lua_pcall( FState, High( Args ) + 1, 1, 0 ) <> 0 then  raise ELuaException.Create( 'Call('+PathToString( Path )+') Lua error : '+lua_tostring( FState, -1) );
    Call := vlua_tovariant( FState, -1, FCallDefVal );
  finally
    lua_pop( FState, 1 );
  end;
end;

function TLuaSystem.Call(const Path: AnsiString; const Args: array of const): Variant;
begin
  if not vlua_getpath( FState, Path ) then raise ELuaException.Create('Call('+Path+') not found!');
  try
    if not lua_isfunction( FState, -1 ) then raise ELuaException.Create('Call('+Path+') not a function!');
    vlua_pusharray( FState, Args );
    if lua_pcall( FState, High( Args ) + 1, 1, 0 ) <> 0 then  raise ELuaException.Create( 'Call('+Path+') Lua error : '+lua_tostring( FState, -1) );
    Call := vlua_tovariant( FState, -1, FCallDefVal );
  finally
    lua_pop( FState, 1 );
  end;
end;

function TLuaSystem.GetProtoTable ( aObj : TObject ) : TLuaTable;
begin
  Exit( TLuaTable.Create( Raw, [ FClassMap[ aObj.ClassName ].Storage, (aObj as ILuaReferencedObject).GetID ] ) );

end;

function TLuaSystem.RunHook(Obj: ILuaReferencedObject; HookName: AnsiString; const Params: array of const): Variant;
begin
  State.Init( Raw );
  RunHook := State.RunHook( Obj, HookName, Params );
end;

function TLuaSystem.ProtectedCall(const Path: array of const; const Args: array of const): Variant;
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

function TLuaSystem.ProtectedCall(const Path: AnsiString; const Args: array of const): Variant;
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

function TLuaSystem.ProtectedRunHook(Obj: ILuaReferencedObject; HookName: AnsiString; const Params: array of const): Variant;
begin
  try
    ProtectedRunHook := FLuaState.RunHook( Obj, HookName, Params );
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

procedure TLuaSystem.Register(const libname: AnsiString; const lr: PluaL_Reg);
begin
  vlua_register( FState, libname, lr );
end;

procedure TLuaSystem.RegisterSubTable ( const aTableName, aSubTable : AnsiString ) ;
begin
  lua_getglobal( FState, aTableName );
    lua_pushstring( FState, PChar(aSubTable) );
    lua_newtable( FState );
    lua_rawset( FState, -3 );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.RegisterMetaTable ( const aTableName : AnsiString;  const aIndexFunc, aNewIndexFunc : lua_CFunction ) ;
begin
  lua_getglobal( FState, aTableName );
  if lua_isnil( FState, -1 ) then
  begin
    lua_pop( FState, 1 );
    lua_createtable( FState, 0, 0 );
    lua_setglobal( FState, aTableName );
    lua_getglobal( FState, aTableName );
  end;
    lua_createtable( FState, 0, 2 );
      lua_pushcfunction( FState, aIndexFunc );
      lua_setfield( FState, -2, '__index' );
      lua_pushcfunction( FState,  aNewIndexFunc );
      lua_setfield( FState, -2, '__newindex' );
    lua_setmetatable( FState, -2 );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.RegisterMetaTable ( const aTableName, aSubTable : AnsiString; const aIndexFunc, aNewIndexFunc : lua_CFunction ) ;
begin
  lua_getglobal( FState, aTableName );
    lua_pushstring( FState, PChar(aSubTable) );
    lua_newtable( FState );
      lua_createtable( FState, 0, 2 );
        lua_pushcfunction( FState, aIndexFunc );
        lua_setfield( FState, -2, '__index' );
        lua_pushcfunction( FState, aNewIndexFunc );
        lua_setfield( FState, -2, '__newindex' );
      lua_setmetatable( FState, -2 );
    lua_rawset( FState, -3 );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.LoadFile( const FileName: AnsiString );
begin
  if luaL_dofile( FState, PChar(FileName) ) <> 0 then
    raise ELuaException.Create( lua_tostring(FState,-1) );
end;

procedure TLuaSystem.LoadStream( IST: TStream; StreamName: AnsiString; Size: DWord );
begin
  Log('Reading "'+StreamName+'" size ('+IntToStr(Size)+'bytes) ('+IntToStr(IST.Position)+'-'+IntToStr(IST.Position+Size)+')');
  if vlua_loadstream( FState, IST, Size, StreamName ) <> 0 then
    begin
      OnError(StreamName+': '+lua_tostring(FState,-1));
      lua_pop(FState,1);
      Exit;
    end;
  FreeAndNil( ISt );
  if lua_pcall(FState, 0, 0, 0)  <> 0 then
  begin
    OnError(StreamName+': '+lua_tostring(FState,-1));
    lua_pop(FState,1);
  end;

  Log('Loaded "'+StreamName+'" ('+IntToStr(Size)+'bytes)');
end;

procedure TLuaSystem.LoadStream(DF: TVDataFile; const StreamName: AnsiString);
var Stream : TStream;
    Size   : Int64;
begin
  Stream := DF.GetFile( StreamName );
  Size   := DF.GetFileSize( StreamName );
  LoadStream( Stream, StreamName, Size );
end;


procedure TLuaSystem.LoadStream(DF: TVDataFile; const DirName, FileName: AnsiString);
var Stream : TStream;
    Size   : Int64;
begin
  Stream := DF.GetFile( FileName, DirName );
  Size   := DF.GetFileSize( FileName, DirName );
  LoadStream( Stream, FileName, Size );
end;

procedure TLuaSystem.OnError(const Message: AnsiString);
begin
  if Assigned( FErrorFunc ) then FErrorFunc( Message );
end;

procedure TLuaSystem.RegisterModule( const ModuleName: AnsiString; DF: TVDataFile );
begin
  FDataFiles[ ModuleName ] := DF;
  FLua.Register('require', @lua_valkyrie_require );
end;

procedure TLuaSystem.RegisterModule( const ModuleName, ModulePath: AnsiString);
begin
  FRawModules[ ModuleName ] := ModulePath;
  FLua.Register('require', @lua_valkyrie_require );
end;

procedure TLuaSystem.RegisterType(AClass: TClass; const ProtoName, StorageName: AnsiString);
begin
  FClassMap[ AClass.ClassName ] := TLuaClassInfo.Create( ProtoName, StorageName );
end;

function TLuaSystem.GetClassInfo ( AClass : TClass ) : TLuaClassInfo;
begin
  GetClassInfo := FClassMap[ AClass.ClassName ];
  Assert( GetClassInfo <> nil );
end;

function TLuaSystem.GetProtoTable(AClass: TClass): AnsiString;
begin
  Exit( FClassMap[ AClass.ClassName ].Proto );
end;

function TLuaSystem.GetStorageTable(AClass: TClass): AnsiString;
begin
  Exit( FClassMap[ AClass.ClassName ].Storage );
end;

function TLuaSystem.RegisterObject(Obj: TObject; aClassName : AnsiString = '' ): Integer;
begin
  if aClassName = '' then aClassName := Obj.ClassName;
  lua_getglobal( FState, FClassMap[ aClassName ].Proto );
  if lua_isnil( FState, -1 ) then raise ELuaException.Create( Obj.ClassName + ' type not registered!' );
  DeepPointerCopy( -1, Obj );
  RegisterObject := luaL_ref( FState, LUA_REGISTRYINDEX );
  lua_pop( FState, 1);
end;

procedure TLuaSystem.UnRegisterObject(Obj: ILuaReferencedObject);
begin
  lua_rawgeti( FState, LUA_REGISTRYINDEX, Obj.GetLuaIndex );
  lua_pushstring( FState, '__ptr' );
  lua_pushboolean( FState, False );
  lua_rawset( FState, -3 );
  lua_pop( FState, 1 );
  luaL_unref( FState, LUA_REGISTRYINDEX, Obj.GetLuaIndex );
end;

function TLuaSystem.GetMemoryKB : DWord;
begin
  Exit( lua_gc( FState, LUA_GCCOUNT, 0 ) );
end;

function TLuaSystem.GetMemoryB : DWord;
begin
  Exit( 1024*lua_gc( FState, LUA_GCCOUNT, 0 ) + lua_gc( FState, LUA_GCCOUNTB, 0 ) );
end;

procedure TLuaSystem.CollectGarbage;
begin
  lua_gc( FState, LUA_GCCOLLECT, 0 );
end;

procedure TLuaSystem.SetPrintFunction ( aPrintFunc : TLuaSystemPrintFunc ) ;
begin
  FPrintFunc := aPrintFunc;
end;

procedure TLuaSystem.Print ( const aText : AnsiString ) ;
begin
  if Assigned( FPrintFunc ) then
    FPrintFunc( aText );
end;

procedure TLuaSystem.ConsoleExecute ( const aCode : AnsiString ) ;
var iError : AnsiString;
    iCode  : Integer;
    iStack : Integer;
    cmd    : AnsiString;
begin
  cmd := Trim(aCode);
  if length(cmd) = 0 then Exit;
  iStack := lua_gettop(FState);
  Print('@B('+IntToStr(iStack)+')> @l'+cmd);

  if cmd[1] = '=' then
  begin
    Delete(cmd,1,1);
    cmd := 'return '+cmd;
  end;

  iCode := luaL_loadstring(FState, PChar(cmd));
  if iCode = 0 then iCode := lua_pcall(FState, 0, LUA_MULTRET, 0);
  if iCode <> 0 then
  begin
    iError := lua_tostring(FState,-1);
    Print('@RError: @l'+iError);
    lua_pop(FState,1);
    Exit;
  end;

  if lua_gettop(FState) > iStack then
  for iCode := iStack+1 to lua_gettop(FState) do
    print_value(FState,iCode);
  lua_settop(FState,iStack);
end;

procedure TLuaSystem.SetErrorFunc ( aErrorFunc : TLuaSystemErrorFunc ) ;
begin
  FErrorFunc := aErrorFunc;
  FLua.FErrorFunc :=  aErrorFunc;
end;

procedure TLuaSystem.TableToStream ( const aPath : AnsiString; aStream : TStream ) ;
begin
  if (not vlua_getpath( FState, aPath )) or (not lua_istable( FState, -1 )) then raise ELuaException.Create('TableToStream('+aPath+') not found!');
  vlua_tabletostream( FState, -1, aStream );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.TableFromStream ( const aPath : AnsiString; aStream : TStream ) ;
begin
  if not vlua_getpath( FState, aPath ) then raise ELuaException.Create('TableFromStream('+aPath+') path not setup!');
  vlua_tablefromstream( FState, -1, aStream );
  lua_pop( FState, 1 );
end;

procedure TLuaSystem.GetPath ( const Path : AnsiString );
var RP   : Word;
begin
  RP := RPos( '.', Path );
  if RP < 1 then
  begin
    lua_push_global( FState );
    lua_pushansistring( FState, Path );
  end
  else
  begin
    if not vlua_getpath( FState, LeftStr( Path, RP-1 ) ) then raise ELuaException.Create('Get('+Path+') failed!');
    lua_pushansistring( FState, Copy( Path, RP+1, Length( Path ) - RP ) );
  end;
end;

procedure TLuaSystem.GetPath ( const Path : array of const );
begin
  Assert( High( Path ) >= 0 );
  if High( Path ) = 0 then
  begin
    lua_push_global( FState );
    vlua_pushvarrec( FState, @Path[0] );
  end
  else
  begin
    if not vlua_getpath( FState, Path, 0, High(Path) - 1 ) then raise ELuaException.Create('Get('+PathToString( Path )+') failed!');
    vlua_pushvarrec( FState, @Path[High( Path )] );
  end;
end;

function TLuaSystem.PathToString(const Path: array of const): AnsiString;
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

procedure TLuaSystem.DeepPointerCopy(Index: Integer; Obj : Pointer );
var HasFunctions : Boolean;
    HasMetatable : Boolean;
begin
  index := lua_absindex( FState, index );
  lua_newtable( FState );
  lua_pushnil( FState );
  HasFunctions := false;
  HasMetatable := false;

  while lua_next( FState, index ) <> 0 do
  begin
    if lua_isfunction( FState, -1 ) then HasFunctions := true
    else if lua_istable( FState, -1 ) then
    begin
      DeepPointerCopy( -1, Obj );
      lua_insert( FState, -2 );
      lua_pop( FState, 1 );
    end;
    lua_pushvalue( FState, -2 );
    lua_insert( FState, -2 );
    lua_settable( FState, -4 );
  end;

  if lua_getmetatable( FState, -2 ) then
  begin
    lua_setmetatable( FState, -2 );
    HasMetatable := true;
  end;

  if HasFunctions or HasMetatable then
  begin
    lua_pushstring( FState, '__ptr' );
    lua_pushlightuserdata( FState, Obj );
    lua_rawset( FState, -3 );
  end;
end;


end.

