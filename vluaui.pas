unit vluaui;
{$mode objfpc}
interface

uses Classes, SysUtils, vioevent, vluastate, vluatable, vuielement, vluatype, vlualibrary, vuitypes;

type TUITableConstructor = function( aTable : TLuaTable; aParent : TUIElement ) : TUIElement;
procedure RegisterUIElementConstructor( const aElementID : AnsiString; aTableConstructor : TUITableConstructor );

procedure RegisterUIClass( L: Plua_State );

procedure vlua_pushioevent( L: Plua_State; const event : TIOEvent );
procedure vlua_pushiokeyevent( L: Plua_State; const event : TIOKeyEvent );
procedure vlua_pushiomouseevent( L: Plua_State; const event : TIOMouseEvent );
procedure vlua_pushiomousemoveevent( L: Plua_State; const event : TIOMouseMoveEvent );
procedure vlua_pushiosystemevent( L: Plua_State; const event : TIOSystemEvent );

function vlua_tostringlist( L : Plua_State; Index : Integer ) : TUIStringArray;

function LuaIOEvent( const event : TIOEvent ) : TLuaType;
function LuaIOKeyEvent( const event : TIOKeyEvent ) : TLuaType;
function LuaIOMouseEvent( const event : TIOMouseEvent ) : TLuaType;
function LuaIOMouseMoveEvent( const event : TIOMouseMoveEvent ) : TLuaType;
function LuaIOSystemEvent( const event : TIOSystemEvent ) : TLuaType;

function CreateLuaUIElement( L: Plua_State; const aID : AnsiString; aParent : TUIElement ) : TUIElement;

implementation

uses vuielements, vnode, vluasystem, vluaext, vutil, vluatools, typinfo, vconui, vconuiext, vgenerics;

procedure RegisterIOConst( L: Plua_State );
var b  : Byte;
    et : TIOEventType;
    mk : TIOModKey;
    mb : TIOMouseButton;
begin
  vlua_gettableorcreate( L, 'io' );
  for b in VKEY_FULLSET do
  begin
    lua_pushansistring( L, 'KEY_'+VKeyToString( b ) );
    lua_pushinteger( L, b );
    lua_rawset( L, -3 );
  end;
  for b := 0 to VIO_SYSEVENT_COUNT do
  begin
    lua_pushansistring( L, 'SYS_'+IOSystemEventNames[ b ] );
    lua_pushinteger( L, b );
    lua_rawset( L, -3 );
  end;
  for et in TIOEventType do
  begin
    lua_pushansistring( L, 'EVENT_'+IOEventTypeToString( et ) );
    lua_pushinteger( L, Byte(et) );
    lua_rawset( L, -3 );
  end;
  for mk in TIOModKey do
  begin
    lua_pushansistring( L, 'MOD_'+VModKeyToString( mk ) );
    lua_pushinteger( L, Byte(mk) );
    lua_rawset( L, -3 );
  end;
  for mb in TIOMouseButton do
  begin
    lua_pushansistring( L, 'MOUSE_'+VMBToString( mb ) );
    lua_pushinteger( L, Byte(mb) );
    lua_rawset( L, -3 );
  end;
  lua_pop( L, 1 );
end;

procedure vlua_pushioevent(L: Plua_State; const event: TIOEvent);
begin
  lua_createtable( L, 0, 2 );
  lua_pushinteger( L, LongInt(event.EType) );
  lua_setfield( L, -2, 'type' );
  case event.EType of
    VEVENT_SYSTEM     : begin vlua_pushiosystemevent( L, event.System ); lua_setfield( L, -2, 'system' ); end;
    VEVENT_KEYDOWN,
    VEVENT_KEYUP      : begin vlua_pushiokeyevent( L, event.Key ); lua_setfield( L, -2, 'key' ); end;
    VEVENT_MOUSEMOVE  : begin vlua_pushiomousemoveevent( L, event.MouseMove ); lua_setfield( L, -2, 'mousemove' ); end;
    VEVENT_MOUSEDOWN,
    VEVENT_MOUSEUP    : begin vlua_pushiomouseevent( L, event.Mouse ); lua_setfield( L, -2, 'mouse' ); end;
  end;
end;

procedure vlua_pushiokeyevent(L: Plua_State; const event: TIOKeyEvent);
var iFlags  : TFlags;
    iModKey : TIOModKey;
begin
  lua_createtable( L, 0, 4 );

  lua_pushinteger( L, event.Code );
  lua_setfield( L, -2, 'code' );

  lua_pushansistring( L, event.ASCII );
  lua_setfield( L, -2, 'ascii' );

  iFlags := [];
  for iModKey in event.ModState do
    Include( iFlags, Byte( iModKey ) );
  vlua_pushflags_set( L, iFlags );
  lua_setfield( L, -2, 'modstate' );

  lua_pushboolean( L, event.Pressed );
  lua_setfield( L, -2, 'pressed' );
end;

procedure vlua_pushiomouseevent(L: Plua_State; const event: TIOMouseEvent);
begin
  lua_createtable( L, 0, 3 );

  lua_pushinteger( L, LongInt(event.Button) );
  lua_setfield( L, -2, 'button' );

  vlua_pushpoint( L, event.Pos );
  lua_setfield( L, -2, 'pos' );

  lua_pushboolean( L, event.Pressed );
  lua_setfield( L, -2, 'pressed' );
end;

procedure vlua_pushiomousemoveevent(L: Plua_State; const event: TIOMouseMoveEvent);
var iFlags : TFlags;
    iMB    : TIOMouseButton;
begin
  lua_createtable( L, 0, 3 );

  iFlags := [];
  for iMB in event.ButtonState do
    Include( iFlags, Byte( iMB ) );
  vlua_pushflags_set( L, iFlags );
  lua_setfield( L, -2, 'buttonstate' );

  vlua_pushpoint( L, event.Pos );
  lua_setfield( L, -2, 'pos' );

  vlua_pushpoint( L, event.RelPos );
  lua_setfield( L, -2, 'relpos' );
end;

procedure vlua_pushiosystemevent(L: Plua_State; const event: TIOSystemEvent);
begin
  lua_createtable( L, 0, 3 );

  lua_pushinteger( L, event.Code );
  lua_setfield( L, -2, 'code' );

  lua_pushinteger( L, event.Param1 );
  lua_setfield( L, -2, 'param1' );

  lua_pushinteger( L, event.Param2 );
  lua_setfield( L, -2, 'param2' );
end;

type

TLuaIOEvent = class( TLuaType )
  constructor Create( aEvent : TIOEvent );
  procedure Push( L : PLua_state ); override;
private
  FEvent : TIOEvent;
end;

TLuaIOKeyEvent = class( TLuaType )
  constructor Create( aEvent : TIOKeyEvent );
  procedure Push( L : PLua_state ); override;
private
  FEvent : TIOKeyEvent;
end;

TLuaIOMouseEvent = class( TLuaType )
  constructor Create( aEvent : TIOMouseEvent );
  procedure Push( L : PLua_state ); override;
private
  FEvent : TIOMouseEvent;
end;

TLuaIOMouseMoveEvent = class( TLuaType )
  constructor Create( aEvent : TIOMouseMoveEvent );
  procedure Push( L : PLua_state ); override;
private
  FEvent : TIOMouseMoveEvent;
end;

TLuaIOSystemEvent = class( TLuaType )
  constructor Create( aEvent : TIOSystemEvent );
  procedure Push( L : PLua_state ); override;
private
  FEvent : TIOSystemEvent;
end;

{ TLuaIOEvent }

constructor TLuaIOEvent.Create(aEvent: TIOEvent);
begin
  FEvent := aEvent;
end;

procedure TLuaIOEvent.Push(L: PLua_state);
begin
  vlua_pushioevent( L, FEvent );
end;

{ TLuaIOKeyEvent }

constructor TLuaIOKeyEvent.Create(aEvent: TIOKeyEvent);
begin
  FEvent := aEvent;
end;

procedure TLuaIOKeyEvent.Push(L: PLua_state);
begin
  vlua_pushiokeyevent( L, FEvent );
end;

{ TLuaIOMouseEvent }

constructor TLuaIOMouseEvent.Create(aEvent: TIOMouseEvent);
begin
  FEvent := aEvent;
end;

procedure TLuaIOMouseEvent.Push(L: PLua_state);
begin
  vlua_pushiomouseevent( L, FEvent );
end;

{ TLuaIOMouseMoveEvent }

constructor TLuaIOMouseMoveEvent.Create(aEvent: TIOMouseMoveEvent);
begin
  FEvent := aEvent;
end;

procedure TLuaIOMouseMoveEvent.Push(L: PLua_state);
begin
  vlua_pushiomousemoveevent( L, FEvent );
end;

{ TLuaIOSystemEvent }

constructor TLuaIOSystemEvent.Create(aEvent: TIOSystemEvent);
begin
  FEvent := aEvent;
end;

procedure TLuaIOSystemEvent.Push(L: PLua_state);
begin
  vlua_pushiosystemevent( L, FEvent );
end;

function vlua_tostringlist(L: Plua_State; Index: Integer): TUIStringArray;
var i : Integer;
begin
  if not lua_istable( L, Index ) then Exit( nil );
  Result := TUIStringArray.Create;
  if lua_objlen( L, Index ) > 0 then
  for i := 1 to lua_objlen( L, Index ) do
  begin
    lua_rawgeti( L, Index, i );
    Result.Push( lua_tostring( L, -1 ) );
    lua_pop( L, 1 );
  end;
end;

function LuaIOEvent(const event: TIOEvent): TLuaType;
begin
  Exit( TLuaIOEvent.Create( event ) );
end;

function LuaIOKeyEvent(const event: TIOKeyEvent): TLuaType;
begin
  Exit( TLuaIOKeyEvent.Create( event ) );
end;

function LuaIOMouseEvent(const event: TIOMouseEvent): TLuaType;
begin
  Exit( TLuaIOMouseEvent.Create( event ) );
end;

function LuaIOMouseMoveEvent(const event: TIOMouseMoveEvent): TLuaType;
begin
  Exit( TLuaIOMouseMoveEvent.Create( event ) );
end;

function LuaIOSystemEvent(const event: TIOSystemEvent): TLuaType;
begin
  Exit( TLuaIOSystemEvent.Create( event ) );
end;

procedure ApplyUIPrototype( L: Plua_State; const aID: AnsiString; aElement : TUIElement );
var iObjIdx : Integer;
    iIndex  : Integer;
    iProp   : AnsiString;
begin
  vlua_pushobject( L, aElement );
  iObjIdx := lua_absindex( L, -1 );
  if not vlua_getpath( L, ['ui_elements', aID]) then raise ELuaException.Create( 'ui_elements.'+aID+' not found!');

  lua_pushstring( L, 'inherited' );
  lua_rawget( L, -2 );
  if lua_isstring( L, -1 ) then
    ApplyUIPrototype( L, lua_tostring( L, -1 ), aElement );
  lua_pop( L, 1 );

  iIndex  := lua_absindex( L, -1 );
  lua_pushnil( L );
  while lua_next( L, iIndex ) <> 0 do
  begin
    if lua_isstring( L, -2 ) then
    begin
      iProp := lua_tostring( L, -2 );
      if UIHookExists( iProp ) then
        begin
          lua_pushvalue( L, -2 ); // push key
          lua_pushvalue( L, -2 ); // push value
          lua_rawset( L, iObjIdx );
          aElement.AddHook( ResolveUIHook( iProp ) );
        end
      else
        if not aElement.SetProperty( L, lua_tostring( L, -2 ), -1 ) then
          if GetPropInfo( aElement.ClassType, iProp ) <> nil then
            SetPropValue( aElement as aElement.ClassType, iProp, vlua_tovariant( L, -1 ) );
    end;
    lua_pop( L, 1 );
  end;

  lua_pop( L, 2 );
end;

function lua_ui_new_element( L: Plua_State ) : Integer; cdecl;
var iState : TLuaState;
begin
  iState.Init( L );
  iState.Push( CreateLuaUIElement( L, iState.ToString( 2 ), iState.ToObject( 1 ) as TUIElement ) );
  Result := 1;
end;

function lua_ui_new_con_label( L: Plua_State ) : Integer; cdecl;
var iState   : TLuaState;
    iElement : TUIElement;
begin
  iState.Init( L );
  iElement := TConUILabel.Create( iState.ToObject( 1 ) as TUIElement, iState.ToPoint( 2 ), iState.ToString( 3, '' ) );
  iElement.RegisterWithLua;
  iState.Push( iElement );
  Result := 1;
end;

function lua_ui_new_con_text( L: Plua_State ) : Integer; cdecl;
var iState   : TLuaState;
    iElement : TUIElement;
begin
  iState.Init( L );
  if iState.IsString( 2 )
    then iElement := TConUIText.Create( iState.ToObject( 1 ) as TUIElement, iState.ToString( 2, '' ) )
    else iElement := TConUIText.Create( iState.ToObject( 1 ) as TUIElement, iState.ToRect( 2 ), iState.ToString( 3, '' ) );
  iElement.RegisterWithLua;
  iState.Push( iElement );
  Result := 1;
end;

function lua_ui_new_con_menu( L: Plua_State ) : Integer; cdecl;
var iState   : TLuaState;
    iElement : TConUIMenu;
    iIndex   : DWord;
begin
  iState.Init( L );
  if iState.IsPoint( 2 )
    then iElement := TConUIMenu.Create( iState.ToObject( 1 ) as TUIElement, iState.ToPoint( 2 ) )
    else iElement := TConUIMenu.Create( iState.ToObject( 1 ) as TUIElement, iState.ToRect( 2 ) );
  iElement.RegisterWithLua;
  iIndex  := lua_absindex( L, -1 );

  lua_pushnil( L );
  while lua_next( L, iIndex ) <> 0 do
  begin
    if lua_type( L, -1 ) = LUA_TSTRING
      then iElement.Add( lua_tostring( L, -1 ) )
      else iElement.Add( iState.GetField(-1,1), iState.GetField(-1,2,True), Pointer(LongInt(iState.GetField(-1,3,0))), iState.GetField(-1,4,0) );
    lua_pop( L, 1 );
  end;

  iState.Push( iElement );
  Result := 1;
end;

function lua_ui_con_menu_set( L: Plua_State ) : Integer; cdecl;
var iState   : TLuaState;
    iElement : TConUIMenu;
    iIndex   : DWord;
begin
  iState.Init( L );
  iElement := iState.ToObject( 1 ) as TConUIMenu;
  iIndex   := iState.ToInteger( 2 );
  with iElement.Items[ iIndex ] do
  begin
    Text   := iState.ToString( 3 );
    if iState.IsBoolean(4) then Active := iState.ToBoolean( 4 );
    if iState.IsNumber(5)  then Data   := Pointer(iState.ToInteger( 5 ));
    if iState.IsNumber(6)  then Color  := iState.ToInteger( 6 );
  end;
  Result := 0;
end;

function lua_ui_new_con_input_line( L: Plua_State ) : Integer; cdecl;
var iState   : TLuaState;
    iElement : TUIElement;
begin
  iState.Init( L );
  iElement := TConUIInputLine.Create( iState.ToObject( 1 ) as TUIElement, iState.ToPoint( 2 ), iState.ToInteger( 3 ) );
  iElement.RegisterWithLua;
  iState.Push( iElement );
  Result := 1;
end;

function lua_ui_new_con_string_list( L: Plua_State ) : Integer; cdecl;
var iState   : TLuaState;
    iElement : TUIElement;
begin
  iState.Init( L );
  if iState.IsTable( 3 )
    then iElement := TConUIStringList.Create( iState.ToObject( 1 ) as TUIElement, iState.ToRect( 2 ), vlua_tostringlist( L, 3 ), True )
    else if iState.IsNil( 3 )
      then iElement := TConUIStringList.Create( iState.ToObject( 1 ) as TUIElement, iState.ToRect( 2 ) )
      else iElement := TConUIStringList.Create( iState.ToObject( 1 ) as TUIElement, iState.ToRect( 2 ), TextFileToUIStringArray( iState.ToString( 3 ) ), True );
  iElement.EventFilter := [ VEVENT_KEYDOWN, VEVENT_MOUSEDOWN ];
  iElement.RegisterWithLua;
  iState.Push( iElement );
  Result := 1;
end;

function lua_ui_new_con_scrollable_icons( L: Plua_State ) : Integer; cdecl;
var iState   : TLuaState;
    iElement : TUIElement;
begin
  iState.Init( L );
  if iState.IsPoint( 4 )
    then iElement := TConUIScrollableIcons.Create( iState.ToObject( 1 ) as TUIElement, iState.ToObject( 2 ) as IUIScrollable, iState.ToRect( 3 ), iState.ToPoint( 4 ) )
    else iElement := TConUIScrollableIcons.Create( iState.ToObject( 1 ) as TUIElement, iState.ToObject( 2 ) as IUIScrollable, iState.ToRect( 3 ) );
  iElement.RegisterWithLua;
  iState.Push( iElement );
  Result := 1;
end;

const lua_ui_lib : array[0..8] of luaL_Reg = (
  ( name : 'new_element';             func: @lua_ui_new_element),
  ( name : 'new_con_label';           func: @lua_ui_new_con_label),
  ( name : 'new_con_text';            func: @lua_ui_new_con_text),
  ( name : 'new_con_menu';            func: @lua_ui_new_con_menu),
  ( name : 'new_con_input_line';      func: @lua_ui_new_con_input_line),
  ( name : 'new_con_string_list';     func: @lua_ui_new_con_string_list),
  ( name : 'new_con_scrollable_icons';func: @lua_ui_new_con_scrollable_icons),
  ( name : 'con_menu_set';            func: @lua_ui_con_menu_set),
  ( name : nil;                       func: nil; )
);

function UIElementConstructor( aTable : TLuaTable; aParent : TUIElement ) : TUIElement;
begin
  if aTable.IsRect( 'rect' )
    then Result := TUIElement.Create( aParent, aTable.GetRect( 'rect' ) )
    else Result := TUIElement.Create( aParent, aParent.GetDimRect )
end;

function ConUIFullWindowConstructor( aTable : TLuaTable; aParent : TUIElement ) : TUIElement;
begin
  Result := TConUIFullWindow.Create( aParent, aTable.GetString('header',''), aTable.GetString('footer','') );
end;

procedure RegisterUIClass(L: Plua_State);
begin
  RegisterIOConst( L );
  TNode.RegisterLuaAPI( 'ui_element' );
  LuaSystem.RegisterType( TUIElement, 'ui_element', 'ui_elements' );
  LuaSystem.Register( 'ui', lua_ui_lib );
  RegisterUIHooksWithLua;
  RegisterUIElementConstructor( 'ui_element',         @UIElementConstructor );
  RegisterUIElementConstructor( 'ui_con_full_window', @ConUIFullWindowConstructor );
end;

type TUIConstructors = specialize TGHashMap< TUITableConstructor >;
var UIConstructors : TUIConstructors;

procedure RegisterUIElementConstructor(const aElementID: AnsiString; aTableConstructor: TUITableConstructor );
begin
  if UIConstructors.Exists( aElementID ) then raise EException.Create( 'Redefinition of UI constructor '+aElementID+'!');
  UIConstructors[ aElementID ] := aTableConstructor;
end;

function CreateLuaUIElement( L: Plua_State; const aID: AnsiString; aParent : TUIElement ): TUIElement;
var iConstructor : TUITableConstructor;
    iTable       : TLuaTable;
    iBase        : AnsiString;
begin
  iConstructor := @UIElementConstructor;
  iTable       := TLuaTable.Create( L, ['ui_elements', aID] );
  try
    if iTable.IsString( 'base' ) then
    begin
      iBase := iTable.GetString( 'base' );
      if UIConstructors.Exists( iBase )
        then iConstructor := UIConstructors[ iBase ]
        else EException.Create( 'UI Constructor '+iBase+' not registered!');
    end;
    Result := iConstructor( iTable, aParent );
  finally
    FreeAndNil( iTable );
  end;
  Result.RegisterWithLua;
  Result.ID := aID;

  ApplyUIPrototype( L, aID, Result );
  Result.RunHook( UIHOOK_ONCREATE, [] );
end;


initialization

UIConstructors := TUIConstructors.Create();

finalization

FreeAndNil( UIConstructors );

end.

