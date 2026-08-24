{$INCLUDE valkyrie.inc}
unit viopadstate;
interface
uses vioevent;

const VPAD_TRIGGER_PRESS_THRESHOLD   = 10000;
      VPAD_TRIGGER_RELEASE_THRESHOLD = 8000;

type TIOPadState = class
  public
    constructor Create;
    procedure Clear;
    function HandleEvent( const aEvent : TIOEvent ) : Boolean;
    function Active( aButton : TIOPadButton ) : Boolean;
  private
    FDown : array[TIOPadButton] of Boolean;
  end;

type TIOPadTriggerState = object
  public
    procedure Clear;
    function Update( const aAxisEvent : TIOPadAxisEvent; out aEvent : TIOEvent ) : Boolean;
  private
    FLeftDown  : Boolean;
    FRightDown : Boolean;
  end;

implementation

constructor TIOPadState.Create;
begin
  Clear;
end;

procedure TIOPadState.Clear;
begin
  FillByte( FDown, SizeOf( FDown ), 0 );
end;

function TIOPadState.HandleEvent( const aEvent : TIOEvent ) : Boolean;
begin
  case aEvent.EType of
    VEVENT_PADDOWN,
    VEVENT_PADUP :
      begin
        FDown[ aEvent.Pad.Button ] := aEvent.Pad.Pressed;
        Exit( True );
      end;
    VEVENT_PADDEVICE :
      begin
        Clear;
        Exit( True );
      end;
  end;
  Exit( False );
end;

function TIOPadState.Active( aButton : TIOPadButton ) : Boolean;
begin
  Result := FDown[ aButton ];
end;

procedure TIOPadTriggerState.Clear;
begin
  FLeftDown  := False;
  FRightDown := False;
end;

function TIOPadTriggerState.Update( const aAxisEvent : TIOPadAxisEvent; out aEvent : TIOEvent ) : Boolean;
var iDown   : PBoolean;
    iButton : TIOPadButton;
begin
  case aAxisEvent.Axis of
    VPAD_AXIS_TRIGGERLEFT :
    begin
      iDown   := @FLeftDown;
      iButton := VPAD_BUTTON_LEFTTRIGGER;
    end;
    VPAD_AXIS_TRIGGERRIGHT :
    begin
      iDown   := @FRightDown;
      iButton := VPAD_BUTTON_RIGHTTRIGGER;
    end;
    else
      Exit( False );
  end;

  if ( not iDown^ ) and ( aAxisEvent.Value > VPAD_TRIGGER_PRESS_THRESHOLD ) then
    iDown^ := True
  else
    if iDown^ and ( aAxisEvent.Value < VPAD_TRIGGER_RELEASE_THRESHOLD ) then
      iDown^ := False
    else
      Exit( False );

  aEvent.Pad.Button  := iButton;
  aEvent.Pad.Pressed := iDown^;
  aEvent.Pad.Which   := aAxisEvent.Which;
  if iDown^
    then aEvent.EType := VEVENT_PADDOWN
    else aEvent.EType := VEVENT_PADUP;
  Exit( True );
end;

end.
