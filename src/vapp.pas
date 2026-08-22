{$INCLUDE valkyrie.inc}
// Generic Valkyrie process shell with option, path, and lifecycle handling.
// Descendants implement LoadConfiguration, PublishPaths, CreateGame,
// DestroyGame, InitializeGame, RunGame, and ShutdownGame.
unit vapp;
interface

uses
  Classes, SysUtils, CustApp;

type TGamePaths = record
    ExecutablePath    : AnsiString;
    ResourcePath      : AnsiString;
    ConfigurationPath : AnsiString;
    SettingsPath      : AnsiString;
    DataPath          : AnsiString;
    WritePath         : AnsiString;
    ScorePath         : AnsiString;
    LogPath           : AnsiString;
    CrashPath         : AnsiString;
    HeapTracePath     : AnsiString;
    procedure Normalize;
    procedure DeriveDiagnostics;
  end;

  TVRunResult = (
    VRR_QUIT,
    VRR_RELOAD_DATA
  );

  TValkyrieApplication = class abstract(TCustomApplication)
  private
    type TOptionSpec = record
      LongName      : AnsiString;
      ShortName     : Char;
      RequiresValue : Boolean;
      ValueName     : AnsiString;
      Description   : AnsiString;
    end;
  private
    FPaths             : TGamePaths;
    FOptionSpecs       : array of TOptionSpec;
    FDataInitialized   : Boolean;
    FShutdown          : Boolean;
    procedure AddStandardOptions;
    procedure ParseOptions;
    procedure WriteHelp;
    procedure ResolvePaths;
    procedure InitializeDiagnostics;
    procedure RunDataGeneration( out aResult : TVRunResult );
    procedure DispatchApplicationException( aException : Exception );
  protected
    procedure AddFlag( const aLongName : AnsiString; aShortName : Char; const aDescription : AnsiString );
    procedure AddValueOption( const aLongName : AnsiString; aShortName : Char; const aValueName, aDescription : AnsiString );
    procedure FailOption( const aMessage : AnsiString );
    procedure DefineOptions; virtual;
    procedure ValidateOptions; virtual;
    procedure DiscoverPaths( var aPaths : TGamePaths ); virtual;
    procedure BeforeConfiguration( var aPaths : TGamePaths ); virtual;
    procedure LoadConfiguration( var aPaths : TGamePaths ); virtual; abstract;
    procedure PublishPaths( const aPaths : TGamePaths ); virtual; abstract;
    procedure ApplyOptions; virtual;
    procedure BeforeDiagnostics; virtual;
    function ExecuteApplicationCommand : Boolean; virtual;

    procedure CreateGame; virtual; abstract;
    procedure DestroyGame; virtual; abstract;
    procedure InitializeGame; virtual; abstract;
    function RunGame : TVRunResult; virtual; abstract;
    procedure ShutdownGame; virtual; abstract;
    procedure ResetGame; virtual;

    procedure GameException( aException : Exception ); virtual;
    procedure ApplicationException( aException : Exception ); virtual;

    procedure DoRun; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Shutdown;
    procedure HandleException( aSender : TObject ); override;

    property Paths : TGamePaths read FPaths;
  end;

implementation

uses
  {$IFDEF HEAPTRACE}heaptrc,{$ENDIF}
  vdebug, vlog, vos, vutil;

const
  CONFIGURATION_FILE = 'config.lua';
  SETTINGS_FILE      = 'settings.lua';
  LOG_FILE           = 'runtime.log';
  CRASH_FILE         = 'error.log';
  HEAP_TRACE_FILE    = 'heap.txt';

procedure TGamePaths.Normalize;
  procedure NormalizeDirectory( var aPath : AnsiString );
  begin
    if aPath = '' then Exit;
    aPath := IncludeTrailingPathDelimiter(
      ExcludeTrailingPathDelimiter(aPath)
    );
  end;
begin
  NormalizeDirectory(ExecutablePath);
  NormalizeDirectory(ResourcePath);
  NormalizeDirectory(DataPath);
  NormalizeDirectory(WritePath);
  NormalizeDirectory(ScorePath);
end;

procedure TGamePaths.DeriveDiagnostics;
begin
  LogPath       := WritePath + LOG_FILE;
  CrashPath     := WritePath + CRASH_FILE;
  HeapTracePath := WritePath + HEAP_TRACE_FILE;
end;

constructor TValkyrieApplication.Create;
begin
  inherited Create(nil);
  OptionChar := '-';
  CaseSensitiveOptions := True;
end;

destructor TValkyrieApplication.Destroy;
begin
  Shutdown;
  inherited Destroy;
end;

procedure TValkyrieApplication.AddFlag( const aLongName : AnsiString; aShortName : Char; const aDescription : AnsiString );
var iIndex : Integer;
    iSpec  : TOptionSpec;
begin
  if aLongName = '' then
    raise EArgumentException.Create('Option long name cannot be empty');

  for iIndex := 0 to High(FOptionSpecs) do
  begin
    if FOptionSpecs[iIndex].LongName = aLongName then
      raise EArgumentException.CreateFmt(
        'Duplicate long option: --%s',
        [aLongName]
      );
    if (aShortName <> #0) and
       (FOptionSpecs[iIndex].ShortName = aShortName) then
      raise EArgumentException.CreateFmt(
        'Duplicate short option: -%s',
        [aShortName]
      );
  end;

  iSpec.LongName      := aLongName;
  iSpec.ShortName     := aShortName;
  iSpec.RequiresValue := False;
  iSpec.ValueName     := '';
  iSpec.Description   := aDescription;
  SetLength(FOptionSpecs, Length(FOptionSpecs) + 1);
  FOptionSpecs[High(FOptionSpecs)] := iSpec;
end;

procedure TValkyrieApplication.AddValueOption( const aLongName : AnsiString; aShortName : Char; const aValueName, aDescription : AnsiString );
var iIndex : Integer;
    iSpec  : TOptionSpec;
begin
  if aLongName = '' then
    raise EArgumentException.Create('Option long name cannot be empty');

  for iIndex := 0 to High(FOptionSpecs) do
  begin
    if FOptionSpecs[iIndex].LongName = aLongName then
      raise EArgumentException.CreateFmt(
        'Duplicate long option: --%s',
        [aLongName]
      );
    if (aShortName <> #0) and
       (FOptionSpecs[iIndex].ShortName = aShortName) then
      raise EArgumentException.CreateFmt(
        'Duplicate short option: -%s',
        [aShortName]
      );
  end;

  iSpec.LongName      := aLongName;
  iSpec.ShortName     := aShortName;
  iSpec.RequiresValue := True;
  iSpec.ValueName     := aValueName;
  iSpec.Description   := aDescription;
  SetLength(FOptionSpecs, Length(FOptionSpecs) + 1);
  FOptionSpecs[High(FOptionSpecs)] := iSpec;
end;

procedure TValkyrieApplication.AddStandardOptions;
begin
  AddFlag(
    'help',
    'h',
    'Show this help and exit.'
  );
  AddValueOption(
    'config',
    #0,
    'FILE',
    'Use FILE as the Lua configuration.'
  );
  AddValueOption(
    'data-path',
    #0,
    'DIR',
    'Override the data/resource directory.'
  );
  AddValueOption(
    'write-path',
    #0,
    'DIR',
    'Override the writable-data directory.'
  );
  AddValueOption(
    'score-path',
    #0,
    'DIR',
    'Override the score directory.'
  );
end;

procedure TValkyrieApplication.ParseOptions;
var iError         : AnsiString;
    iIndex         : Integer;
    iLongOptions   : TStringList;
    iNonOptions    : TStringList;
    iParsedOptions : TStringList;
    iShortOptions  : AnsiString;
begin
  iShortOptions := '';
  iLongOptions := TStringList.Create;
  iNonOptions := TStringList.Create;
  iParsedOptions := TStringList.Create;
  try
    for iIndex := 0 to High(FOptionSpecs) do
    begin
      if FOptionSpecs[iIndex].ShortName <> #0 then
      begin
        iShortOptions := iShortOptions + FOptionSpecs[iIndex].ShortName;
        if FOptionSpecs[iIndex].RequiresValue then
          iShortOptions := iShortOptions + ':';
      end;

      if FOptionSpecs[iIndex].RequiresValue then
        iLongOptions.Add(FOptionSpecs[iIndex].LongName + ':')
      else
        iLongOptions.Add(FOptionSpecs[iIndex].LongName);
    end;

    iError := CheckOptions(
      iShortOptions,
      iLongOptions,
      iParsedOptions,
      iNonOptions,
      True
    );

    if iError <> '' then
    begin
      WriteLn(StdErr, iError);
      WriteLn(StdErr, 'Try "', ExeName, ' --help" for usage.');
      Terminate(2);
      Exit;
    end;

    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate(0);
      Exit;
    end;

    if iNonOptions.Count > 0 then
    begin
      WriteLn(StdErr, 'Unexpected argument: ', iNonOptions[0]);
      Terminate(2);
    end;
  finally
    iParsedOptions.Free;
    iNonOptions.Free;
    iLongOptions.Free;
  end;
end;

procedure TValkyrieApplication.WriteHelp;
var iIndex     : Integer;
    iMaxLength : Integer;
    iSpelling  : AnsiString;
  function OptionSpelling( const aSpec : TOptionSpec ) : AnsiString;
  begin
    if aSpec.ShortName = #0 then
      Result := '    --' + aSpec.LongName
    else
      Result := '-' + aSpec.ShortName + ', --' + aSpec.LongName;
    if aSpec.RequiresValue then
      Result := Result + '=' + aSpec.ValueName;
  end;
begin
  iMaxLength := 0;
  for iIndex := 0 to High(FOptionSpecs) do
  begin
    iSpelling := OptionSpelling(FOptionSpecs[iIndex]);
    if Length(iSpelling) > iMaxLength then
      iMaxLength := Length(iSpelling);
  end;

  WriteLn('Usage: ', ExeName, ' [options]');
  WriteLn;
  WriteLn('Options:');
  for iIndex := 0 to High(FOptionSpecs) do
  begin
    iSpelling := OptionSpelling(FOptionSpecs[iIndex]);
    WriteLn(
      '  ',
      iSpelling,
      StringOfChar(' ', iMaxLength - Length(iSpelling) + 3),
      FOptionSpecs[iIndex].Description
    );
  end;
end;

procedure TValkyrieApplication.FailOption( const aMessage : AnsiString );
begin
  WriteLn(StdErr, aMessage);
  WriteLn(StdErr, 'Try "', ExeName, ' --help" for usage.');
  Terminate(2);
end;

procedure TValkyrieApplication.DefineOptions;
begin
end;

procedure TValkyrieApplication.ValidateOptions;
begin
end;

procedure TValkyrieApplication.DiscoverPaths( var aPaths : TGamePaths );
var iCandidate : AnsiString;
begin
  aPaths.ExecutablePath := ExtractFilePath(ExeName);
  iCandidate := GetResourcesPath;
  if (iCandidate <> '') and
     (not FileExists(iCandidate + CONFIGURATION_FILE)) then
    iCandidate := '';

  aPaths.ResourcePath      := iCandidate;
  aPaths.ConfigurationPath := iCandidate + CONFIGURATION_FILE;
  aPaths.SettingsPath      := iCandidate + SETTINGS_FILE;
  aPaths.DataPath          := iCandidate;
  aPaths.WritePath         := '';
  aPaths.ScorePath         := '';
end;

procedure TValkyrieApplication.BeforeConfiguration( var aPaths : TGamePaths );
begin
end;

procedure TValkyrieApplication.ResolvePaths;
begin
  DiscoverPaths(FPaths);
  if HasOption('config') then
    FPaths.ConfigurationPath := GetOptionValue('config');

  PublishPaths(FPaths);
  BeforeConfiguration(FPaths);
  PublishPaths(FPaths);
  LoadConfiguration(FPaths);

  if HasOption('data-path') then
    FPaths.DataPath := GetOptionValue('data-path');
  if HasOption('write-path') then
    FPaths.WritePath := GetOptionValue('write-path');
  if HasOption('score-path') then
    FPaths.ScorePath := GetOptionValue('score-path');

  FPaths.Normalize;
  FPaths.DeriveDiagnostics;
  PublishPaths(FPaths);
end;

procedure TValkyrieApplication.ApplyOptions;
begin
end;

procedure TValkyrieApplication.BeforeDiagnostics;
begin
end;

function TValkyrieApplication.ExecuteApplicationCommand : Boolean;
begin
  Result := False;
end;

procedure TValkyrieApplication.ResetGame;
begin
end;

procedure TValkyrieApplication.GameException( aException : Exception );
begin
end;

procedure TValkyrieApplication.ApplicationException( aException : Exception );
begin
end;

procedure TValkyrieApplication.InitializeDiagnostics;
begin
  {$IFDEF HEAPTRACE}
  SetHeapTraceOutput(FPaths.HeapTracePath);
  {$ENDIF}
  ErrorLogFileName := FPaths.CrashPath;
  BeforeDiagnostics;
  Logger.AddSink(
    TTextFileLogSink.Create(LOGDEBUG, FPaths.LogPath, False)
  );
  LogSystemInfo;
  Logger.Log(LOGINFO, 'Write path set to - ' + FPaths.WritePath);
  Logger.Log(LOGINFO, 'Log path set to - ' + FPaths.LogPath);
end;

procedure TValkyrieApplication.Initialize;
begin
  try
    inherited Initialize;
    AddStandardOptions;
    DefineOptions;
    ParseOptions;
    if Terminated then Exit;
    ValidateOptions;
    if Terminated then Exit;

    ResolvePaths;
    ApplyOptions;
    InitializeDiagnostics;
  except
    on E : Exception do
    begin
      DispatchApplicationException(E);
      raise;
    end;
  end;
end;

procedure TValkyrieApplication.RunDataGeneration( out aResult : TVRunResult );
begin
  InitializeGame;
  FDataInitialized := True;
  try
    try
      aResult := RunGame;
    except
      on E : Exception do
      begin
        GameException(E);
        raise;
      end;
    end;
  finally
    try
      ShutdownGame;
    finally
      FDataInitialized := False;
    end;
  end;
end;

procedure TValkyrieApplication.DispatchApplicationException( aException : Exception );
begin
  if Assigned(Logger) then
    Logger.Flush;
  ApplicationException(aException);
end;

procedure TValkyrieApplication.DoRun;
var iResult : TVRunResult;
begin
  if Terminated then Exit;
  try
    if ExecuteApplicationCommand then
    begin
      Terminate(0);
      Exit;
    end;

    CreateGame;
    repeat
      RunDataGeneration(iResult);
      if iResult = VRR_RELOAD_DATA then
        ResetGame;
    until iResult = VRR_QUIT;
    Terminate(0);
  finally
    Shutdown;
  end;
end;

procedure TValkyrieApplication.Shutdown;
begin
  if FShutdown then Exit;
  FShutdown := True;
  try
    if FDataInitialized then
      try
        ShutdownGame;
      finally
        FDataInitialized := False;
      end;
  finally
    try
      DestroyGame;
    finally
      if Assigned(Logger) then
        Logger.Flush;
    end;
  end;
end;

procedure TValkyrieApplication.HandleException( aSender : TObject );
begin
  inherited HandleException(aSender);
end;

end.
