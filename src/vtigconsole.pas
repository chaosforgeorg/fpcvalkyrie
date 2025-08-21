{$INCLUDE valkyrie.inc}
unit vtigconsole;
interface
uses viotypes, vgenerics;

type TTIGStringRing = specialize TGRingBuffer<AnsiString>;

type TTIGConsoleView = class( TIOLayer )
  constructor Create;
  procedure Update( aDTime : Integer ); override;
  function IsFinished : Boolean; override;
  function IsModal : Boolean; override;
  procedure Writeln( const aText : Ansistring );
  procedure Finish;
  procedure LoadHistory( const aFileName : AnsiString );
  procedure SaveHistory( const aFileName : AnsiString );
  destructor Destroy; override;
protected
  procedure Execute( const aLine : Ansistring );
protected
  FFinished : Boolean;
  FHPos     : DWord;
  FText     : TTIGStringRing;
  FHistory  : TTIGStringRing;
  FInput    : array[0..74] of Char;
end;

implementation

uses sysutils, classes, vutil, vtig, vtigio, vio, vluasystem;

const TIG_CONSOLE_LINES = 16;

constructor TTIGConsoleView.Create;
begin
  FHistory  := nil;
  FFinished := False;
  FText     := TTIGStringRing.Create( TIG_CONSOLE_LINES );
  FInput[0] := #0;
  if LuaSystem <> nil then
     LuaSystem.SetPrintFunction( @Writeln );
  IO.Driver.StartTextInput;
end;

procedure TTIGConsoleView.Update( aDTime : Integer );
var iLine : Ansistring;
    i     : Integer;
begin
  VTIG_Begin( 'tig_console', Point( IO.Console.SizeX, TIG_CONSOLE_LINES + 4 ), Point(1,1) );
  if FText.Size < TIG_CONSOLE_LINES then
    for i := 1 to TIG_CONSOLE_LINES - FText.Size do
      VTIG_Text('');
  for iLine in FText do
    VTIG_Text( iLine );
  iLine := '';
  if VTIG_Input(@FInput[0],74,True) then
    iLine := AnsiString( FInput );
  VTIG_End();

  if iLine = '' then
  begin
    if VTIG_GetIOState.EventState.Activated( VTIG_IE_UP, true )   then
    begin
      Inc( FHPos );
      StrPLCopy(@FInput[0], FHistory.Get( -FHPos ), High(FInput));
    end;
    if VTIG_GetIOState.EventState.Activated( VTIG_IE_DOWN, true ) then
    begin
      Dec( FHPos );
      StrPLCopy(@FInput[0], FHistory.Get( -FHPos ), High(FInput));
    end;
  end
  else Execute( iLine );
end;

procedure TTIGConsoleView.Writeln( const aText : Ansistring );
var iPos : Integer;
begin
  iPos := 1;
  while iPos <= Length( aText ) do
  begin
    FText.PushBack( Copy( aText, iPos, 74 ) );
    iPos += 74;
  end;
end;

function TTIGConsoleView.IsFinished : Boolean;
begin
  Exit( FFinished );
end;

function TTIGConsoleView.IsModal : Boolean;
begin
  Exit( True );
end;

procedure TTIGConsoleView.Finish;
begin
  FFinished := True;
end;

procedure TTIGConsoleView.LoadHistory ( const aFileName : AnsiString ) ;
var iStream : TStream;
begin
  FreeAndNil( FHistory );
  if FileExists( aFileName ) then
  begin
    try
      iStream := TFileStream.Create( aFileName, fmOpenRead );
      FHistory := TTIGStringRing.CreateFromStream( iStream );
    finally
      FreeAndNil( iStream );
    end;
  end;
  if FHistory = nil then FHistory := TTIGStringRing.Create(256);
end;

procedure TTIGConsoleView.SaveHistory ( const aFileName : AnsiString ) ;
var iStream : TStream;
begin
  if FHistory <> nil then
  try
    iStream := TFileStream.Create( aFileName, fmCreate );
    FHistory.WriteToStream( iStream );
  finally
    FreeAndNil( iStream );
  end;
end;

procedure TTIGConsoleView.Execute( const aLine : Ansistring );
begin
  FText.PushBack( aLine );
  FInput[0] := #0;
  if Assigned( FHistory ) then
    if not ((FHistory.Size > 0) and (FHistory.Back = aLine)) then
      FHistory.PushBack( aLine );
  FHPos  := 0;
  try
    LuaSystem.ConsoleExecute( aLine );
  except on E : Exception do
  end;
end;

destructor TTIGConsoleView.Destroy;
begin
  IO.Driver.StopTextInput;
  FreeAndNil( FHistory );
  FreeAndNil( FText );
end;

end.

