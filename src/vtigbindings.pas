{$INCLUDE valkyrie.inc}
// Standard VTIG keyboard and controller bindings
unit vtigbindings;
interface
uses vbindings, vioevent, vioeventstate, vtigio;

const VTIG_KEY_BINDING_GROUP = 'ui_bindings_keyboard';
      VTIG_PAD_BINDING_GROUP = 'ui_bindings_controller';

const VTIGKeyBindingInfo : array[0..13] of TBindingInfo = (
  ( Action: VTIG_IE_UP;        ID: 'ui_keyboard_up';        Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_UP;                         Name: 'Up';        Description: 'Move the UI selection or view up.' ),
  ( Action: VTIG_IE_DOWN;      ID: 'ui_keyboard_down';      Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_DOWN;                       Name: 'Down';      Description: 'Move the UI selection or view down.' ),
  ( Action: VTIG_IE_LEFT;      ID: 'ui_keyboard_left';      Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_LEFT;                       Name: 'Left';      Description: 'Move the UI selection, tab, or caret left.' ),
  ( Action: VTIG_IE_RIGHT;     ID: 'ui_keyboard_right';     Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_RIGHT;                      Name: 'Right';     Description: 'Move the UI selection, tab, or caret right.' ),
  ( Action: VTIG_IE_HOME;      ID: 'ui_keyboard_home';      Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_HOME;                       Name: 'Home';      Description: 'Move to the start of a UI list or text field.' ),
  ( Action: VTIG_IE_END;       ID: 'ui_keyboard_end';       Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_END;                        Name: 'End';       Description: 'Move to the end of a UI list or text field.' ),
  ( Action: VTIG_IE_PGUP;      ID: 'ui_keyboard_page_up';   Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_PGUP;                       Name: 'Page up';   Description: 'Move a UI view up by one page.' ),
  ( Action: VTIG_IE_PGDOWN;    ID: 'ui_keyboard_page_down'; Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_PGDOWN;                     Name: 'Page down'; Description: 'Move a UI view down by one page.' ),
  ( Action: VTIG_IE_CANCEL;    ID: 'ui_keyboard_cancel';    Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_ESCAPE;                     Name: 'Cancel';    Description: 'Cancel or leave the current UI.' ),
  ( Action: VTIG_IE_CONFIRM;   ID: 'ui_keyboard_confirm';   Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_ENTER;                      Name: 'Confirm';   Description: 'Confirm the current UI selection.' ),
  ( Action: VTIG_IE_SELECT;    ID: 'ui_keyboard_select';    Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_SPACE;                      Name: 'Select';    Description: 'Select the current UI entry.' ),
  ( Action: VTIG_IE_BACKSPACE; ID: 'ui_keyboard_backspace'; Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_BACK;                       Name: 'Backspace'; Description: 'Delete or perform the primary UI alternate action.' ),
  ( Action: VTIG_IE_TAB;       ID: 'ui_keyboard_tab';       Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_TAB;                        Name: 'Tab';       Description: 'Switch the current UI tab or item.' ),
  ( Action: VTIG_IE_DELETE;    ID: 'ui_keyboard_delete';    Group: VTIG_KEY_BINDING_GROUP; Default: VKEY_DELETE;                     Name: 'Delete';    Description: 'Delete at the current text caret.' )
);

const VTIGPadBindingInfo : array[0..7] of TBindingInfo = (
  ( Action: VTIG_IE_UP;        ID: 'ui_controller_up';        Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_DPAD_UP);    Name: 'Up';        Description: 'Move the UI selection or view up.' ),
  ( Action: VTIG_IE_DOWN;      ID: 'ui_controller_down';      Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_DPAD_DOWN);  Name: 'Down';      Description: 'Move the UI selection or view down.' ),
  ( Action: VTIG_IE_LEFT;      ID: 'ui_controller_left';      Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_DPAD_LEFT);  Name: 'Left';      Description: 'Move the UI selection or tab left.' ),
  ( Action: VTIG_IE_RIGHT;     ID: 'ui_controller_right';     Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_DPAD_RIGHT); Name: 'Right';     Description: 'Move the UI selection or tab right.' ),
  ( Action: VTIG_IE_CANCEL;    ID: 'ui_controller_cancel';    Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_B);          Name: 'Cancel';    Description: 'Cancel or leave the current UI.' ),
  ( Action: VTIG_IE_CONFIRM;   ID: 'ui_controller_confirm';   Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_A);          Name: 'Confirm';   Description: 'Confirm the current UI selection.' ),
  ( Action: VTIG_IE_BACKSPACE; ID: 'ui_controller_backspace'; Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_Y);          Name: 'Backspace'; Description: 'Perform the primary UI alternate action.' ),
  ( Action: VTIG_IE_TAB;       ID: 'ui_controller_tab';       Group: VTIG_PAD_BINDING_GROUP; Default: Ord(VPAD_BUTTON_X);          Name: 'Tab';       Description: 'Switch the current UI tab or item.' )
);

procedure VTIG_LoadDefaultBindings( aContext : TBindingContext );
procedure VTIG_ApplyDefaultPadAliases( aContext : TBindingContext;
  aLeft, aRight : Boolean );
procedure VTIG_ApplyBindingEvent( const aEvent : TIOEvent;
  aContext : TBindingContext; aState : TIOEventState );

implementation

procedure VTIG_ApplyDefaultPadAliases( aContext : TBindingContext;
  aLeft, aRight : Boolean );
begin
  if aLeft and
     ( aContext.ResolvePad( VPAD_BUTTON_LEFTSHOULDER ) = BINDING_NONE ) then
    aContext.BindPad( VPAD_BUTTON_LEFTSHOULDER, VTIG_IE_LEFT );
  if aRight and
     ( aContext.ResolvePad( VPAD_BUTTON_RIGHTSHOULDER ) = BINDING_NONE ) then
    aContext.BindPad( VPAD_BUTTON_RIGHTSHOULDER, VTIG_IE_RIGHT );
end;

procedure VTIG_LoadDefaultBindings( aContext : TBindingContext );
var iInfo : TBindingInfo;
begin
  aContext.Clear;
  for iInfo in VTIGKeyBindingInfo do
    aContext.BindKey( TIOKeyCode( iInfo.Default ), iInfo.Action );
  for iInfo in VTIGPadBindingInfo do
    aContext.BindPad( TIOPadButton( iInfo.Default ), iInfo.Action );
  VTIG_ApplyDefaultPadAliases( aContext, True, True );
end;

procedure VTIG_ApplyBindingEvent( const aEvent : TIOEvent;
  aContext : TBindingContext; aState : TIOEventState );
var iAction : TBindingAction;
begin
  case aEvent.EType of
    VEVENT_KEYDOWN,
    VEVENT_KEYUP:
      begin
        if aEvent.Key.Repeated then Exit;
        iAction := aContext.ResolveKey( TIOKeyCode( aEvent.Key.Code ) );
        if ( iAction >= 0 ) and ( iAction < VIO_MAXEVENTS ) then
          aState.SetState( iAction, aEvent.Key.Pressed );
      end;
    VEVENT_PADDOWN,
    VEVENT_PADUP:
      begin
        iAction := aContext.ResolvePad( aEvent.Pad.Button );
        if ( iAction >= 0 ) and ( iAction < VIO_MAXEVENTS ) then
          aState.SetState( iAction, aEvent.Pad.Pressed );
      end;
  end;
end;

end.
