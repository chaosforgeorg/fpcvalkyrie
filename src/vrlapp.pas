{$INCLUDE valkyrie.inc}
// Shared roguelike application and runtime lifecycle contracts.
unit vrlapp;
interface

uses sysutils,
     vapp, vsystem, vrandom, viorl, vluasystem;

type
  ERLRuntimeState = class( Exception );

// TRLRuntime
//
// Architectural boundary: owns reusable roguelike services and their
// data-generation lifecycle. Game policy belongs in descendants, while
// one-playthrough state belongs in a game session or controller.
type TRLRuntime = class abstract( TSystem )
  private
    FConfiguration   : TObject;
    FGameRNG         : TRNG;
    FIO              : TIORL;
    FLua             : TLuaSystem;
    FDataInitialized : Boolean;
    procedure ReleaseLua;
  protected
    FPaths : TGamePaths;
    function CreateIO : TIORL; virtual; abstract;
    function CreateLua : TLuaSystem; virtual; abstract;
    procedure PrepareGameData; virtual;
    procedure InitializeGameData; virtual;
    function RunGame : TVRunResult; virtual; abstract;
    procedure ShutdownGameData; virtual;
    procedure ResetGameData; virtual;
  public
    constructor Create( const aPaths : TGamePaths; var aConfiguration : TObject ); reintroduce; virtual;
    destructor Destroy; override;
    procedure Initialize;
    function Run : TVRunResult;
    procedure Shutdown;
    procedure Reset;
    procedure HandleGameException( aException : Exception ); virtual;
    procedure ReplaceGameRNG( var aGameRNG : TRNG );

    property Paths : TGamePaths read FPaths;
    property Configuration : TObject read FConfiguration;
    property GameRNG : TRNG read FGameRNG;
    property IO : TIORL read FIO;
    property Lua : TLuaSystem read FLua;
    property DataInitialized : Boolean read FDataInitialized;
  end;

// TRLApplication
//
// Architectural boundary: owns process bootstrap, paths, configuration
// creation, and exactly one runtime. Loaded game data and playthrough state
// belong to the runtime and its active session.
type TRLApplication = class abstract( TValkyrieApplication )
  private
    FConfiguration : TObject;
    FRuntime       : TRLRuntime;
  protected
    function CreateConfiguration( var aPaths : TGamePaths ) : TObject; virtual; abstract;
    function CreateRuntime( const aPaths : TGamePaths; var aConfiguration : TObject ) : TRLRuntime; virtual; abstract;

    procedure LoadConfiguration( var aPaths : TGamePaths ); override;
    procedure ExecuteApplication; override;
    procedure FinalizeApplication; override;

    property Configuration : TObject read FConfiguration;
    property Runtime : TRLRuntime read FRuntime;
  end;

implementation

uses
  vio, vlua;

{ TRLRuntime }

constructor TRLRuntime.Create( const aPaths : TGamePaths; var aConfiguration : TObject );
begin
  inherited Create;
  FPaths := aPaths;
  FConfiguration := aConfiguration;
  aConfiguration := nil;

  FGameRNG := TRNG.Create(0);
  LuaRNG := FGameRNG;

  FIO := CreateIO;
  if FIO <> nil then
    Add(FIO);
end;

destructor TRLRuntime.Destroy;
begin
  ReleaseLua;

  if vio.IO = FIO then
    vio.IO := nil;
  FreeAndNil(FIO);

  if LuaRNG = FGameRNG then
    LuaRNG := nil;
  FreeAndNil(FGameRNG);
  FreeAndNil(FConfiguration);
  inherited Destroy;
end;

procedure TRLRuntime.ReleaseLua;
begin
  if LuaSystem = FLua then
    LuaSystem := nil;
  FreeAndNil(FLua);
end;

procedure TRLRuntime.InitializeGameData;
begin
end;

procedure TRLRuntime.PrepareGameData;
begin
end;

procedure TRLRuntime.ShutdownGameData;
begin
end;

procedure TRLRuntime.ResetGameData;
begin
end;

// Initialize brackets Lua creation between pre-Lua and post-Lua data hooks.
procedure TRLRuntime.Initialize;
begin
  if FDataInitialized then
    raise ERLRuntimeState.Create('Runtime data is already initialized');

  try
    PrepareGameData;
    FLua := CreateLua;
    if FLua <> nil then
      Add(FLua);
    LuaSystem := FLua;
    InitializeGameData;
    FDataInitialized := True;
  except
    try
      ShutdownGameData;
    except
    end;
    FDataInitialized := False;
    ReleaseLua;
    raise;
  end;
end;

function TRLRuntime.Run : TVRunResult;
begin
  if not FDataInitialized then
    raise ERLRuntimeState.Create('Runtime data is not initialized');
  Result := RunGame;
end;

// Shutdown invokes game cleanup before unpublishing and freeing Lua.
procedure TRLRuntime.Shutdown;
begin
  if not FDataInitialized then Exit;
  try
    ShutdownGameData;
  finally
    FDataInitialized := False;
    ReleaseLua;
  end;
end;

// Reset is valid only between initialized data generations.
procedure TRLRuntime.Reset;
begin
  if FDataInitialized then
    raise ERLRuntimeState.Create('Cannot reset initialized runtime data');
  ResetGameData;
end;

procedure TRLRuntime.HandleGameException( aException : Exception );
begin
end;

procedure TRLRuntime.ReplaceGameRNG( var aGameRNG : TRNG );
var iPrevious : TRNG;
begin
  if aGameRNG = nil then
    raise ERLRuntimeState.Create('Replacement game RNG is nil');
  if aGameRNG = FGameRNG then
  begin
    aGameRNG := nil;
    Exit;
  end;
  iPrevious := FGameRNG;
  FGameRNG := aGameRNG;
  aGameRNG := nil;
  LuaRNG := FGameRNG;
  iPrevious.Free;
end;

{ TRLApplication }

procedure TRLApplication.LoadConfiguration( var aPaths : TGamePaths );
begin
  FConfiguration := CreateConfiguration(aPaths);
end;

// create runtime -> initialize data -> run -> shutdown data -> optional reset
procedure TRLApplication.ExecuteApplication;
var iResult : TVRunResult;
begin
  FRuntime := CreateRuntime(FPaths, FConfiguration);
  if FRuntime = nil then
    raise Exception.Create('CreateRuntime returned nil');
  if FConfiguration <> nil then
    raise Exception.Create('CreateRuntime did not take configuration ownership');
  repeat
    FRuntime.Initialize;
    try
      try
        iResult := FRuntime.Run;
      except
        on E : Exception do
        begin
          FRuntime.HandleGameException(E);
          raise;
        end;
      end;
    finally
      FRuntime.Shutdown;
    end;
    if iResult = VRR_RELOAD_DATA then
      FRuntime.Reset;
  until iResult = VRR_QUIT;
end;

procedure TRLApplication.FinalizeApplication;
begin
  FreeAndNil(FRuntime);
  FreeAndNil(FConfiguration);
end;

end.
