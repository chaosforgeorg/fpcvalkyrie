{$INCLUDE valkyrie.inc}
unit vioeventstate;
interface
uses vutil;

const VIO_MAXEVENTS = 256;
      VIO_MAXINPUT  = 16;

type TIOEventState = class
  public
    constructor Create;
    procedure Update( aElapsed: Single );
    procedure SetState( aState: Integer; aValue: Boolean );
    procedure AppendText( aText: PWideChar );
    function Activated( aState: Integer; aRepeat: Boolean = False ): Boolean;
    function Activated( aStates : TFlags ): Boolean;
    function Active( aState: Integer ): Boolean;
    procedure EndFrame;
    procedure Clear;
  private
    FDuration    : array[0..VIO_MAXEVENTS - 1] of Single;
    FDown        : array[0..VIO_MAXEVENTS - 1] of Boolean;
    FPressed     : array[0..VIO_MAXEVENTS - 1] of Boolean;
    FInput       : array[0..VIO_MAXINPUT - 1]  of WChar;
    FRepeatDelay : Single;
    FRepeatRate  : Single;
    FLastElapsed : Single;
  private
    function GetInput : PWideChar;
  public
    property RepeatDelay : Single read FRepeatDelay write FRepeatDelay;
    property RepeatRate  : Single read FRepeatRate  write FRepeatRate;
    property Input       : PWideChar read GetInput;
  end;

implementation

constructor TIOEventState.Create;
begin
  FRepeatDelay := 0.3;
  FRepeatRate  := 0.1;
  FLastElapsed := 0.0;
  Clear;
end;

procedure TIOEventState.Update( aElapsed: Single );
var i : Integer;
begin
  for i := 0 to VIO_MAXEVENTS - 1 do
  begin
    // An initial press lasts one update, even when no time has elapsed.
    FPressed[i] := FDown[i] and ( FDuration[i] < 0.0 );
    if FDown[i] then
    begin
      if FDuration[i] < 0.0 then
        FDuration[i] := 0.0
      else
        FDuration[i] += aElapsed;
    end
    else
      FDuration[i] := -1.0;
  end;
  FLastElapsed := aElapsed;
end;

procedure TIOEventState.SetState( aState: Integer; aValue: Boolean );
begin
  FDuration[aState] := -1.0;
  FDown[aState] := aValue;
  FPressed[aState] := False;
end;

procedure TIOEventState.AppendText( aText: PWideChar );
var n, i : Integer;
begin
  n := 0;
  while ( n < VIO_MAXINPUT ) and ( FInput[n] <> #0 ) do
    Inc( n );

  i := 0;
  while ( n + 1 < VIO_MAXINPUT ) and ( aText[i] <> #0 ) do
  begin
    FInput[n] := aText[i];
    Inc(n);
    Inc(i);
  end;

  if n < VIO_MAXINPUT then
    FInput[n] := #0;
end;

function TIOEventState.Activated( aState: Integer; aRepeat: Boolean = False ): Boolean;
var
  duration: Single;
begin
  if not FDown[aState] then
    Exit(False);

  if FPressed[aState] then
    Exit( True );

  if aRepeat and (FRepeatRate > 0.0) then
  begin
    duration := FDuration[aState] - FRepeatDelay;
    if duration >= 0.0 then
    begin
      // Activate at the delay boundary, then once per complete repeat interval.
      if ( duration - FLastElapsed < 0.0 ) or
         ( Trunc( duration / FRepeatRate ) >
           Trunc( ( duration - FLastElapsed ) / FRepeatRate ) ) then
        Exit( True );
    end;
  end;
  Result := False;
end;

function TIOEventState.Activated( aStates : TFlags ): Boolean;
var iState : Byte;
begin
  for iState in aStates do
    if FDown[iState] and FPressed[iState] then
      Exit( True );
  Exit( False );
end;

function TIOEventState.GetInput: PWideChar;
begin
  Result := @FInput[0];
end;

function TIOEventState.Active( aState: Integer ): Boolean;
begin
  Result := FDown[aState];
end;

procedure TIOEventState.EndFrame;
var
  i : Integer;
begin
  for i := 0 to VIO_MAXINPUT - 1 do
    FInput[i] := #0;
end;

procedure TIOEventState.Clear;
var i : Integer;
begin
  for i := 0 to VIO_MAXEVENTS - 1 do
  begin
    FDuration[i] := -1.0;
    FDown[i] := False;
    FPressed[i] := False;
  end;
  for i := 0 to VIO_MAXINPUT - 1 do
    FInput[i] := #0;
end;

end.

