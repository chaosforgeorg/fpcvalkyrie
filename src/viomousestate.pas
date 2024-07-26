{$INCLUDE valkyrie.inc}
unit viomousestate;
interface
uses viotypes, vioevent;

const VIO_MAXBUTTONS = 5;

type TIOButtonInfo = record
  Duration     : Single;
  PrevDuration : Single;
  DragSquared  : Single;
  Position     : TIOPoint;
  Time         : Single;
  Down         : Boolean;
  Clicked      : Boolean;
  DClicked     : Boolean;
  Released     : Boolean;
  FastClick    : Boolean;
end;


type TIOMouseState = class
  public
    constructor Create;
    procedure Update( aElapsed: Single );
    procedure EndFrame;
    function HandleEvent( const aEvent: TIOEvent ): Boolean;
    function Info( aCode: TIOMouseButton ): TIOButtonInfo;
    function Down( aCode: TIOMouseButton ): Boolean;
    function Released( aCode: TIOMouseButton ): Boolean;
    function Clicked( aCode: TIOMouseButton ): Boolean;
    function ClickTime( aCode: TIOMouseButton ): Single;
    function DoubleClicked( aCode: TIOMouseButton ): Boolean;
    function IsDragging( aCode: TIOMouseButton = VMB_BUTTON_LEFT; aThreshold: Single = -1.0 ): Boolean;
    function DragDelta( aCode: TIOMouseButton = VMB_BUTTON_LEFT; aThreshold: Single = -1.0 ): TIOPoint;
    procedure ResetDelta( aCode: TIOMouseButton = VMB_BUTTON_LEFT );
  private
    FButtons       : array[0..VIO_MAXBUTTONS - 1] of TIOButtonInfo;

    FPosition      : TIOPoint;
    FPrevPosition  : TIOPoint;
    FDelta         : TIOPoint;
    FWheel         : TIOPoint;

    FTime          : Single;

    FDClickTime    : Single;
    FDClickDist    : Single;
    FDragThreshold : Single;
    FMoved         : Boolean;
  private
    procedure Clear;
  public
    property DClickTime : Single    read FDClickTime    write FDClickTime;
    property DClickDist : Single    read FDClickDist    write FDClickDist;
    property DragThreshold : Single read FDragThreshold write FDragThreshold;
    property Position : TIOPoint    read FPosition;
    property Delta : TIOPoint       read FDelta;
    property Wheel : TIOPoint       read FWheel;
    property Moved : Boolean        read FMoved;
  end;

implementation

uses Math, vutil;

constructor TIOMouseState.Create;
begin
  FPosition      := Point(-1, -1);
  FPrevPosition  := Point(-1, -1);
  FDelta         := Point(-1, -1);
  FWheel         := Point(0, 0);
  FDClickTime    := 0.3;
  FDClickDist    := 6.0;
  FDragThreshold := 6.0;
  FTime          := 0.0;
  FMoved         := False;
  // Initialize button info
  Clear;
end;

procedure TIOMouseState.Clear;
var i : Integer;
begin
  for i := 0 to VIO_MAXBUTTONS - 1 do
  begin
    FButtons[i].Duration    := -1.0;
    FButtons[i].PrevDuration:= -1.0;
    FButtons[i].DragSquared := 0.0;
    FButtons[i].Position    := Point(-1, -1);
    FButtons[i].Time        := 0.0;
    FButtons[i].Down        := False;
    FButtons[i].Clicked     := False;
    FButtons[i].DClicked    := False;
    FButtons[i].Released    := False;
    FButtons[i].FastClick   := False;
  end;
end;

procedure TIOMouseState.Update( aElapsed: Single );
var i       : Integer;
    iButton : TIOButtonInfo;
begin
  if ( FPosition.X < 0 ) and ( FPosition.Y < 0 ) then
    FPosition := Point( -9999, -9999 );
  if ( (FPosition.X < 0) and (FPosition.Y < 0) ) or ( (FPrevPosition.X < 0) and (FPrevPosition.Y < 0) ) then
    FDelta := Point( 0, 0 )
  else
    FDelta := Point( FPosition.X - FPrevPosition.X, FPosition.Y - FPrevPosition.Y );
  FPrevPosition := FPosition;

  for i := 0 to VIO_MAXBUTTONS - 1 do
  begin
    iButton := FButtons[i];
    iButton.Clicked      := iButton.Down and (iButton.Duration < 0.0);
    iButton.Released     := not iButton.Down and (iButton.Duration >= 0.0);
    iButton.PrevDuration := iButton.Duration;
    if iButton.Down then
    begin
      if iButton.Duration < 0.0 then
        iButton.Duration := 0.0
      else
        iButton.Duration += aElapsed;
    end
    else
      iButton.Duration := -1.0;
    iButton.DClicked := False;
    if iButton.Clicked then
    begin
      if FTime - iButton.Time < FDClickTime then
      begin
        if Sqr( FPosition.X - iButton.Position.X ) + Sqr( FPosition.Y - iButton.Position.Y ) < Sqr( FDClickDist ) then
          iButton.DClicked := True;
        iButton.Time := -9999.0; // so the third click isn't turned into a double-click
      end
      else
        iButton.Time := FTime;
      iButton.Position    := FPosition;
      iButton.DragSquared := 0.0;
    end
    else
    if iButton.Down then
      iButton.DragSquared := Max( iButton.DragSquared, Sqr( FPosition.X - iButton.Position.X ) + Sqr( FPosition.Y - iButton.Position.Y ) );
    FButtons[i] := iButton;
  end;
  FTime := FTime + aElapsed;
end;

function TIOMouseState.HandleEvent( const aEvent: TIOEvent ): Boolean;
begin
  case aEvent.EType of
    VEVENT_MOUSEMOVE:
      begin
        FPosition := aEvent.MouseMove.Pos;
        FMoved    := True;
        Result    := True;
      end;
    VEVENT_MOUSEDOWN,
    VEVENT_MOUSEUP :
      begin
        case aEvent.Mouse.Button of
          VMB_BUTTON_LEFT   : FButtons[0].Down := aEvent.Mouse.Pressed or FButtons[0].FastClick;
          VMB_BUTTON_MIDDLE : FButtons[1].Down := aEvent.Mouse.Pressed or FButtons[1].FastClick;
          VMB_BUTTON_RIGHT  : FButtons[2].Down := aEvent.Mouse.Pressed or FButtons[2].FastClick;
          VMB_WHEEL_UP      : begin FWheel := FWheel + Point( 0, -3 ); Exit( True ); end;
          VMB_WHEEL_DOWN    : begin FWheel := FWheel + Point( 0, 3 ); Exit( True ); end;
        end;
        // TODO: is this Ord value correct?
        FButtons[Ord(aEvent.Mouse.Button) - 1].FastClick := True;
        Result := True;
      end;
  else
    Result := False;
  end;
end;

procedure TIOMouseState.EndFrame;
var i : Integer;
begin
  FMoved := False;
  FWheel := Point( 0, 0 );
  for i := 0 to VIO_MAXBUTTONS - 1 do
    FButtons[i].FastClick := False;
end;

function TIOMouseState.Info( aCode : TIOMouseButton ) : TIOButtonInfo;
begin
  Result := FButtons[Ord(aCode) - 1];
end;

function TIOMouseState.Down( aCode : TIOMouseButton ): Boolean;
begin
  Result := FButtons[Ord(aCode) - 1].Down;
end;

function TIOMouseState.Released( aCode : TIOMouseButton ): Boolean;
begin
  Result := FButtons[Ord(aCode) - 1].Released;
end;

function TIOMouseState.Clicked( aCode : TIOMouseButton ): Boolean;
begin
  Result := FButtons[Ord(aCode) - 1].Duration = 0.0;
end;

function TIOMouseState.ClickTime( aCode : TIOMouseButton ): Single;
begin
  Result := FButtons[Ord(aCode) - 1].Time;
end;

function TIOMouseState.DoubleClicked( aCode : TIOMouseButton ): Boolean;
begin
  Result := FButtons[Ord(aCode) - 1].DClicked;
end;

function TIOMouseState.IsDragging( aCode : TIOMouseButton; aThreshold : Single ): Boolean;
var iButton : TIOButtonInfo;
begin
  iButton := FButtons[Ord(aCode) - 1];
  if not iButton.Down then
    Exit( False );
  if aThreshold < 0.0 then
    aThreshold := FDragThreshold;
  Result := iButton.DragSquared >= Sqr( aThreshold );
end;

function TIOMouseState.DragDelta( aCode : TIOMouseButton; aThreshold : Single ): TIOPoint;
var iButton : TIOButtonInfo;
begin
  iButton := FButtons[Ord(aCode) - 1];
  if aThreshold < 0.0 then
    aThreshold := FDragThreshold;
  if iButton.Down and ( iButton.DragSquared >= Sqr( aThreshold ) ) then
    Exit( Point( FPosition.X - iButton.Position.X, FPosition.Y - iButton.Position.Y ) );
  Result := Point(0, 0);
end;

procedure TIOMouseState.ResetDelta( aCode : TIOMouseButton );
begin
  FButtons[Ord(aCode) - 1].Position := FPosition;
end;

end.

