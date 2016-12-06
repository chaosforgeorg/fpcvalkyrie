{$INCLUDE valkyrie.inc}
// @abstract(Unique Identification class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
//
// Logging unit. Enables logging to different sinks (targets). Supplied
// sinks include:
//
//  TConsoleLogSink  -- enables logging to console, supports colored logging
//  TTextFileLogSink -- enables logging to text file, supports auto-flush and appending
//  TCallbackLogSink -- enables logging via a passed callback
//
// Unit auto-initializes, and auto-deinitializes the logger variable.
unit vlog;
interface
uses Classes, SysUtils, vutil, vgenerics;

type
  // Log callback type for TCallbackLogSink.
  TLogCallback = procedure ( aLevel: TLogLevel; const aMessage : AnsiString) of object;

const
  // Log level name color if colored output is supported.
  LogLevelColor   : array[TLogLevel] of Byte = ( White, LightGray, LightRed, Yellow, LightCyan, LightMagenta, Magenta);
  // Log message color if colored output is not supported.
  LogNormalColor  : array[TLogLevel] of Byte = ( White, White, White, White, White, White, White );
  // Log level name.
  LogLevelName    : array[TLogLevel] of AnsiString = ( 'NONE', 'REPORT', 'ERROR', 'WARNING', 'INFO', 'DEBUG', 'DEBUG2' );
  // Log level name.
  LogLevelNamePad : array[TLogLevel] of AnsiString = ( 'NONE   ', 'REPORT ', 'ERROR  ', 'WARNING', 'INFO   ', 'DEBUG  ', 'DEBUG2 ' );

type
  // Abstract log sink.
  //
  // Override this class to implement a new log sink. As each log sink has it's
  // own log level that can be different from others, also on each logging to the
  // sink the value of the level is checked.
  TLogSink = class(TObject)
  public
    // Constructor.
    //
    // Sets the log sink level.
    //
    // @param(level is the minimum log level to be accepted by this sink)
    constructor Create( aLevel: TLogLevel );

    // Log function.
    //
    // Checks the log level, if proper then passes the log message to the
    // log implementation (logImpl).
    //
    // @param(level is the log level of the message)
    // @param(msg is message to be logged)
    procedure Log( aLevel: TLogLevel; const aMessage : AnsiString );

    // Destructor.
    //
    // Does nothing, can be overriden.
    destructor Destroy; override;

  protected
    // Stored log level
    FLogLevel : TLogLevel;
    FEnabled  : boolean;

  protected
    // Log level implementation.
    //
    // Must be overriden by the implementation.
    //
    // @param(level is the log level of the message - already checked)
    // @param(msg is message to be logged)
    procedure LogImpl( aLevel: TLogLevel; const aMessage : AnsiString); virtual; abstract;
  public
    // Public property for reading the log level.
    property LogLevel : TLogLevel read FLogLevel write FLogLevel;
    property Enabled  : boolean   read FEnabled  write FEnabled;
  end;

  // Console log sink.
  //
  // This is a log sink that logs it's messages to the console. No checking is
  // done to see wether the console actually exists. By default the log sink logs
  // in color (currently only supported under Windows), but this can be changed
  // in the constructor or via the ColorEnabled property.
  TConsoleLogSink = class(TLogSink)
  public
    // Constructor.
    //
    // Passes the level to the log sink, and optionally can be passed with
    // the ColorEnabled flag.
    //
    // @param(level is the minimum log level to be accepted by this sink)
    // @param(Color states wether color output is enabled, on by default)
    constructor Create( aLevel : TLogLevel; aColor : Boolean = True );

  protected
    // Color output flag
    FColorEnabled : boolean;

  protected
    // Implementation of the logging mechanism
    procedure LogImpl( aLevel: TLogLevel; const aMessage : AnsiString ); override;
    // Internal color setting
    procedure SetColor( aColor : Byte );

  public
    // Public property for modyfing and accessing the color output flag.
    property ColorEnabled : Boolean Read FColorEnabled Write FColorEnabled;
  end;

  // Callback log sink.
  //
  // This is a log sink that enables log messages to be passed to a user
  // callback, for example for printing to a form in the application. Note that
  // the callback *MUST* be always valid. Once the callback is not valid (e.g.
  // form has been destroyed, manually set the Callback to nil (you must store
  // a pointer to the callback log sink to do that).
  TCallbackLogSink = class(TLogSink)
  public
    // Constructor.
    //
    // Passes the level to the log sink, and optionally can be passed with
    // the Callback.
    //
    // @param(level is the minimum log level to be accepted by this sink)
    // @param(Callback is the callback that will be called)
    constructor Create( aLevel : TLogLevel; aLogCallback : TLogCallback = nil );

  protected
    // Stored callback
    FLogCallback : TLogCallback;

  protected
    // Implementation of the logging mechanism, logs only if the callback is
    // not nil
    procedure LogImpl( aLevel: TLogLevel; const aMessage : AnsiString ); override;

  public
    // Public property for modyfing and accessing the callback.
    property Callback : TLogCallback Read FLogCallback Write FLogCallback;
  end;

  // Text file Log sink.
  //
  // This is a log sink that logs it's messages to a text file. By default the
  // sink creates a new file when it's created, but if the constructor is passed
  // the AppendFile parameter it will append to an existing file if it exists.
  //
  // By default, the file is flushed when needed, if monitoring is needed, the
  // file can be flushed on each message (however this is slow!).
  TTextFileLogSink = class(TLogSink)
  public
    // Constructor.
    //
    // Passes the level to the log sink, and optionally can set advanced
    // parameters of the Text file log sink. If a file exists it will be
    // overwritten (or appended if AppendFile is true), if not, it will be
    // created.
    //
    // @param(aLevel is the minimum log level to be accepted by this sink)
    // @param(aFileName is the name of the file to log to)
    // @param(aAppend states whether to append to the file instead of rewriting)
    // @param(aFlush states whether to flush after each write (slow!))
    constructor Create( aLevel: TLogLevel; const aFileName : AnsiString; aFlush : Boolean );

    // Destructor.
    //
    // Flushes and closes the file.
    destructor Destroy; override;

    procedure Flush;
  protected
    // Text file - TODO: move to FileOpen
    FText       : Text;
    // Flush and append flag
    FFlush      : Boolean;
    // File name
    FFileName   : AnsiString;

  protected
    // Implementation of the text file logging
    procedure LogImpl( aLevel : TLogLevel; const aMessage : AnsiString); override;
  public
    // Public property for reading the file name
    property FileName : AnsiString read FFileName;
  end;


  // Array of LogSinks.
  TLogSinks = specialize TGObjectArray<TLogSink>;

  // Logger singleton.
  //
  // This global class (used via the auto-initialized "logger" variable) allows
  // logging on different levels to different sources. To log anything, needs
  // to be passed a log sink. Log sinks are taken ownership of, and disposed
  // of at destruction.
  TLogger = class(TObject)
  protected
    // Maximum log level of stored sinks, initially LOGNONE.
    FLogLevel     : TLogLevel;
    // Stored log sinks.
    FLogSinks     : TLogSinks;
    // number of log calls
    FLogCounter   : QWord;
    // Logger enable?
    FEnabled      : boolean;
  public
    // Constructor.
    //
    // Initializes the member fields. Until a sink is added, all log messages
    // are discarded.
    constructor Create;

    // Add a sink to the logger.
    //
    // Adds a sink to the logging mechanism. From now on, up till destruction,
    // all log messages will be passed to that sink. LogLevel of the Logger
    // is adjusted if passed sink has a higher log level. Automatically
    // called at unit initialization, should not be called manually.
    //
    // @param(ASink is the sink to be added, deletion will be handled by Logger)
    procedure AddSink( ASink : TLogSink );

    // Log message.
    //
    // Message is passed to corresponing sinks that have the passed level
    // for processing.
    //
    // @param(level is the log level of the message)
    // @param(msg is message to be logged)
    procedure Log( aLevel: TLogLevel; const aMessage : AnsiString ); overload;

    // Log message with formatting.
    //
    // Message is passed to corresponing sinks that have the passed level
    // for processing. Formatting via Format is done beforehand. All exceptions
    // are caught and reported.
    //
    // @param(level is the log level of the message)
    // @param(msg is message to be logged with formatting chars)
    // @param(Data is the data to be injected into the message)
    procedure Log( aLevel: TLogLevel; const aMessage : AnsiString; const aData: array of const); overload;

    // Sets a global log level.
    //
    // All stored sinks will have this log level from now on.
    //
    // @param(level is the desired global log level)
    procedure SetGlobalLogLevel( aLevel : TLogLevel);

    // Destructor.
    //
    // Frees all loggers and shuts down the logging mechanism. Automatically
    // called at unit deinitialization, should not be called manually.
    destructor Destroy; override;

     // flush associated logfiles to disk
    procedure Flush;

  public
    // Public property for reading the highest log level stored.
    property LogLevel: TLogLevel Read FLogLevel Write SetGlobalLogLevel;
    // number of log calls
    property LogCounter: uint64 read FLogCounter;
  end;

// Logger singleton
var Logger : TLogger = nil;

//procedure AddLogFileSink(fileName : string; append, constantFlush : boolean);
//procedure createLogger(appendToExistingFile : boolean);
procedure LogSystemInfo();

implementation

{$IFDEF WINDOWS}
uses
// Include windows for console coloring
Windows;
{$ELSE}
// probably not needed
{$ENDIF}

{ TLogSink }

constructor TLogSink.Create( aLevel : TLogLevel );
begin
  inherited Create;

  // set the level
  FLogLevel := aLevel;

  // Enable the sink by default
  FEnabled := true;
end;

procedure TLogSink.Log( aLevel: TLogLevel; const aMessage : AnsiString );
begin
  // Check if we are on the proper log level, if not, exit; that the sink is enabled
  if (aLevel > FLogLevel) or (not FEnabled) then Exit;

  // Logging ok, call implementation
  LogImpl( aLevel, aMessage );
end;


destructor TLogSink.Destroy;
begin
  inherited Destroy;
end;

{ TConsoleLogSink }

constructor TConsoleLogSink.Create( aLevel : TLogLevel; aColor : Boolean );
begin
  inherited Create( aLevel );
  FColorEnabled := aColor;
end;

procedure TConsoleLogSink.LogImpl( aLevel : TLogLevel; const aMessage : AnsiString );
begin
  try
    // If color is enabled, use change color between level and msg
    if FColorEnabled then
    begin
      Write( TimeToStr(now) + '  ');
      SetColor( LogLevelColor[aLevel] );
      Write( format('%-8s:  ', [LogLevelName[aLevel]]));
      SetColor( LogNormalColor[aLevel] );
      WriteLn( aMessage );
      SetColor( LightGray );
    end
    else
      WriteLn( LogLevelName[aLevel] + ': ' + aMessage );
  except
  end;
end;

procedure TConsoleLogSink.SetColor(aColor: byte);
begin
{$IFDEF WINDOWS}
  // Set console color using windows API
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), aColor);
{$ELSE}
  // TODO: Ansi escape sequences for non-Windows platforms
{$ENDIF}
end;

{ TCallbackLogSink }

constructor TCallbackLogSink.Create ( aLevel : TLogLevel; aLogCallback : TLogCallback ) ;
begin
  inherited Create( aLevel );
  FLogCallback := aLogCallback;
end;

procedure TCallbackLogSink.LogImpl ( aLevel : TLogLevel; const aMessage : AnsiString ) ;
begin
  if Assigned(FLogCallback) then
    FLogCallback( aLevel, aMessage );
end;

{ TTextFileLogSink }

constructor TTextFileLogSink.Create ( aLevel : TLogLevel; const aFileName : AnsiString; aFlush : Boolean ) ;
begin
  inherited Create( aLevel );

  // Set filename
  FFileName   := aFileName;
  // Set flush mode
  FFlush      := aFlush;

  Assign( FText, FFileName );
  Rewrite( FText );

  LogImpl( LOGINFO, '--- Logging start : '+TimeStamp+' ---' );
end;

destructor TTextFileLogSink.Destroy;
begin
  Flush;
  Close( FText );
  inherited Destroy;
end;

procedure TTextFileLogSink.Flush;
begin
  System.Flush( FText );
end;

procedure TTextFileLogSink.LogImpl ( aLevel : TLogLevel; const aMessage : AnsiString ) ;
begin
  WriteLn( FText, TimeToStr(now)+' : '+LogLevelNamePad[aLevel] + ' : ' + aMessage );
  if FFlush then Flush;
end;

{ TLogger }

constructor TLogger.Create;
begin
  FEnabled      := true;
  FLogLevel     := LOGNONE;
  FLogSinks     := TLogSinks.Create;
  FLogCounter   := 0;
end;

procedure TLogger.AddSink ( ASink : TLogSink ) ;
begin
  // Assure that we have the proper global log level
  if aSink.LogLevel > FLogLevel then FLogLevel := aSink.LogLevel;

  // Add the sink to the sink list
  FLogSinks.Push( aSink );
end;

procedure TLogger.Log ( aLevel : TLogLevel; const aMessage : AnsiString ) ;
var iSink : TLogSink;
begin
  if not FEnabled then exit;
  Inc(FLogCounter);

  // Do not log, if the level is above the global log level
  if aLevel > FLogLevel then Exit;

  // Log to all sinks
  for iSink in FLogSinks do
    iSink.Log( aLevel, aMessage );
end;


procedure TLogger.Log ( aLevel : TLogLevel; const aMessage : AnsiString;
  const aData : array of const ) ;
begin
  if not FEnabled then exit;

  // Check log level, if not enough exit.
  // Note : this check is also made in log, and in every LogSink, however,
  //   it's cheaper to do the check than to do a needless Format call.
  if aLevel > FLogLevel then exit;

  // Try the loging
  try
    Log( aLevel, Format( aMessage, aData ) );
  except
    // catch conversion exceptions
    on e : EConvertError do
      Log( LOGWARN, 'Badly formatted log message -- "' + aMessage + '"!');
    // catch all other exceptions
    on e : Exception do
      Log( LOGWARN, 'Unhandled '+e.ClassName+' exception while logging -- "' + e.Message + '"!');
  end;

end;

procedure TLogger.SetGlobalLogLevel ( aLevel : TLogLevel ) ;
var iSink : TLogSink;
begin
  Log( LOGINFO, 'Setting LogLevel = %s', [LogLevelName[aLevel]]);
  FLogLevel:=alevel;

  // If we have any logsinks, set their log level
  for iSink in FLogSinks do
    iSink.LogLevel := aLevel;
end;

destructor TLogger.Destroy;
begin
  FEnabled      := false;

  // Free the log sinks
  FreeAndNil(FLogSinks);

  inherited Destroy;
end;

procedure TLogger.Flush;
var iSink : TLogSink;
begin
  for iSink in FLogSinks do
    if iSink is TTextFileLogSink then
      TTextFileLogSink(iSink).Flush;
end;

procedure LogSystemInfo;
begin
  Logger.log(LOGREPORT, '---------------------- Compiler info -------------------------');
  Logger.log(LOGREPORT, 'This program was compiled at %s on %s by %s', [{$I %TIME%}, {$I %DATE%}, {$I %USER%}]);
  Logger.log(LOGREPORT, 'Compiler version  : %s', [{$I %FPCVERSION%}]);
  Logger.log(LOGREPORT, 'Target OS         : %s', [{$I %FPCTARGETOS%}]);
  Logger.log(LOGREPORT, 'Target CPU        : %s', [{$I %FPCTARGETCPU%}]);
  Logger.log(LOGREPORT, '---------------------- OS runtime info -----------------------');
  Logger.log(LOGREPORT, 'Current dir       : %s', [GetCurrentDir()]);
  Logger.log(LOGREPORT, 'Local time        : %s', [FormatDateTime('MM/DD/YYYY hh:nn:ss', now())]);
end;

initialization
  // At initialization, create the logger, and add two log sinks
  Logger := TLogger.Create;

// At deinitialization, free the logger (and log sinks).
finalization

  if Assigned( Logger ) then
  begin
    Logger.Log( LOGINFO, '--- Logging end   : '+TimeStamp+' ---' );
    FreeAndNil( Logger );
  end;

end.

