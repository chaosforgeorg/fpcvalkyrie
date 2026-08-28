{$INCLUDE valkyrie.inc}
// Shared roguelike application and runtime lifecycle contracts.
unit vrlapp;
interface

uses sysutils,
     vapp, vsystem, vrandom, viorl, vluasystem;

type
  ERLRuntimeState = class( Exception );

type

{ TRLRuntime }

  TRLRuntime = class abstract( TSystem )
  private
    FPaths           : TGamePaths;
    FConfiguration   : TObject;
    FGameRNG         : TRNG;
    FIO              : TIORL;
    FLua             : TLuaSystem;
    FDataInitialized : Boolean;
  protected
    function CreateIO : TIORL; virtual; abstract;
    function CreateLua : TLuaSystem; virtual; abstract;
    procedure InitializeGameData; virtual;
    function RunGame : TVRunResult; virtual; abstract;
    procedure ShutdownGameData; virtual;
    procedure ResetGameData; virtual;
    procedure GameException( aException : Exception ); virtual;
  public
    constructor Create( const aPaths : TGamePaths; var aConfiguration : TObject ); reintroduce; virtual;
    destructor Destroy; override;
    procedure Initialize;
    function Run : TVRunResult;
    procedure Shutdown;
    procedure Reset;
    procedure HandleGameException( aException : Exception );

    property Paths : TGamePaths read FPaths;
    property Configuration : TObject read FConfiguration;
    property GameRNG : TRNG read FGameRNG;
    property IO : TIORL read FIO;
    property Lua : TLuaSystem read FLua;
    property DataInitialized : Boolean read FDataInitialized;
  end;

{ TRLApplication }

  TRLApplication = class abstract( TValkyrieApplication )
  private
    FConfiguration : TObject;
    FRuntime       : TRLRuntime;
  protected
    function CreateConfiguration( var aPaths : TGamePaths ) : TObject; virtual; abstract;
    function CreateRuntime( const aPaths : TGamePaths; var aConfiguration : TObject ) : TRLRuntime; virtual; abstract;
    function ExecuteGameUtility : Boolean; virtual;

    procedure LoadConfiguration( var aPaths : TGamePaths ); override;
    function ExecuteApplicationCommand : Boolean; override;
    procedure CreateGame; override;
    procedure DestroyGame; override;
    procedure InitializeGame; override;
    function RunGame : TVRunResult; override;
    procedure ShutdownGame; override;
    procedure ResetGame; override;
    procedure GameException( aException : Exception ); override;
  public
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
  if LuaSystem = FLua then
    LuaSystem := nil;
  FreeAndNil(FLua);

  if vio.IO = FIO then
    vio.IO := nil;
  FreeAndNil(FIO);

  if LuaRNG = FGameRNG then
    LuaRNG := nil;
  FreeAndNil(FGameRNG);
  FreeAndNil(FConfiguration);
  inherited Destroy;
end;

procedure TRLRuntime.InitializeGameData;
begin
end;

procedure TRLRuntime.ShutdownGameData;
begin
end;

procedure TRLRuntime.ResetGameData;
begin
end;

procedure TRLRuntime.GameException( aException : Exception );
begin
end;

procedure TRLRuntime.Initialize;
begin
  if FDataInitialized then
    raise ERLRuntimeState.Create('Runtime data is already initialized');

  FLua := CreateLua;
  try
    if FLua <> nil then
      Add(FLua);
    LuaSystem := FLua;
    InitializeGameData;
    FDataInitialized := True;
  except
    if LuaSystem = FLua then
      LuaSystem := nil;
    FreeAndNil(FLua);
    raise;
  end;
end;

function TRLRuntime.Run : TVRunResult;
begin
  if not FDataInitialized then
    raise ERLRuntimeState.Create('Runtime data is not initialized');
  Result := RunGame;
end;

procedure TRLRuntime.Shutdown;
begin
  if not FDataInitialized then Exit;
  try
    ShutdownGameData;
  finally
    FDataInitialized := False;
    if LuaSystem = FLua then
      LuaSystem := nil;
    FreeAndNil(FLua);
  end;
end;

procedure TRLRuntime.Reset;
begin
  if FDataInitialized then
    raise ERLRuntimeState.Create('Cannot reset initialized runtime data');
  ResetGameData;
end;

procedure TRLRuntime.HandleGameException( aException : Exception );
begin
  GameException(aException);
end;

{ TRLApplication }

function TRLApplication.ExecuteGameUtility : Boolean;
begin
  Result := False;
end;

procedure TRLApplication.LoadConfiguration( var aPaths : TGamePaths );
begin
  FConfiguration := CreateConfiguration(aPaths);
end;

function TRLApplication.ExecuteApplicationCommand : Boolean;
begin
  Result := ExecuteGameUtility;
end;

procedure TRLApplication.CreateGame;
begin
  FRuntime := CreateRuntime(FPaths, FConfiguration);
  if FRuntime = nil then
    raise Exception.Create('CreateRuntime returned nil');
  if FConfiguration <> nil then
    raise Exception.Create('CreateRuntime did not take configuration ownership');
end;

procedure TRLApplication.DestroyGame;
begin
  FreeAndNil(FRuntime);
  FreeAndNil(FConfiguration);
end;

procedure TRLApplication.InitializeGame;
begin
  FRuntime.Initialize;
end;

function TRLApplication.RunGame : TVRunResult;
begin
  Result := FRuntime.Run;
end;

procedure TRLApplication.ShutdownGame;
begin
  FRuntime.Shutdown;
end;

procedure TRLApplication.ResetGame;
begin
  FRuntime.Reset;
end;

procedure TRLApplication.GameException( aException : Exception );
begin
  if FRuntime <> nil then
    FRuntime.HandleGameException(aException);
  inherited GameException(aException);
end;

end.
