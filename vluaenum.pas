{$INCLUDE valkyrie.inc}
// @abstract(Lua enum for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(December 4, 2011)
//
// THIS UNIT IS EXPERIMENTAL!
unit vluaenum;
interface
uses lua, typinfo;

procedure RegisterEnumClass( L: Plua_State );
procedure RegisterEnum( L: Plua_State; EnumTypeInfo: PTypeInfo; const EnumName : AnsiString; Strip : Byte = 0; UpperCase : Boolean = True );

implementation

uses classes, sysutils;

const VALKYRIE_ENUM             = 'valkyrie.enum';
      VALKYRIE_ENUM_CLASS_NAME  = 'enum';

function lua_enum_new( L: Plua_State ): Integer; cdecl;
var i : Integer;
begin
  lua_settop( L, 1 );
  lua_newtable( L );
  if lua_istable( L, 1 ) then
  begin
    i := 0;
    while True do
    begin
      Inc(i);
      lua_rawgeti(L, 1, i);
      if lua_isnil( L, -1 ) then
      begin
	lua_pop( L, 1 );
	break;
      end;
      lua_pushinteger( L, i-1 );
      lua_rawset( L, 2 );
    end;
  end;
  luaL_getmetatable( L, VALKYRIE_ENUM );
  lua_setmetatable( L, -2 );
  Exit( 1 );
end;

function lua_enum_index( L: Plua_State ): Integer; cdecl;
begin
  lua_settop( L, 2 );
  lua_rawget( L, 1 );
  if lua_isnil( L, -1 ) then
    luaL_argerror( L, 2, 'enumeration value invalid!' );
  Exit( 1 );
end;

function lua_enum_newindex( L: Plua_State ): Integer; cdecl;
begin
  luaL_error( L, 'enumeration table is read-only!' );
  Exit( 0 );
end;

const enumlib_f : array[0..1] of luaL_Reg = (
  ( name : 'new'; func : @lua_enum_new ),
  ( name : nil;   func : nil )
);

const enumlib_m : array[0..2] of luaL_Reg = (
  ( name : '__index';    func : @lua_enum_index ),
  ( name : '__newindex'; func : @lua_enum_newindex ),
  ( name : nil;   func : nil )
);

procedure RegisterEnumClass ( L : Plua_State ) ;
begin
  luaL_newmetatable( L, VALKYRIE_ENUM );
  luaL_register( L, nil, enumlib_m );
  luaL_register( L, VALKYRIE_ENUM_CLASS_NAME, enumlib_f );
  lua_pop( L, 1 );
end;

procedure RegisterEnum ( L : Plua_State; EnumTypeInfo : PTypeInfo; const EnumName : AnsiString; Strip : Byte; UpperCase : Boolean ) ;
var Count, i : DWord;
    Name     : AnsiString;
    Value    : Integer;
begin
  Count := GetEnumNameCount( EnumTypeInfo );
  lua_createtable( L, 0, Count );
  if Count > 0 then
  for i := 0 to Count-1 do
  begin
    Name  := GetEnumName( EnumTypeInfo, i );
    Value := GetEnumValue( EnumTypeInfo, Name );
    if UpperCase then Name := UpCase(Name);
    if Strip <> 0 then Delete( Name, 1, Strip );
    lua_pushstring( L, Name );
    lua_pushinteger( L, Value );
    lua_rawset( L, -3 );
  end;
  luaL_getmetatable( L, VALKYRIE_ENUM );
  lua_setmetatable( L, -2 );
  lua_setglobal( L, EnumName );
end;

end.

