unit vlibrary;
{$include valkyrie.inc}
interface

uses
  {$IFDEF UNIX} unix, dl, {$ENDIF}
  sysutils, dynlibs;


type
  ELibraryError = class( Exception );

  {$WARNINGS OFF}
  TLibrary = class
  public
    class function Load( const aName: AnsiString; aErrorReport : boolean = true ) : TLibrary;
  public
    destructor Destroy; override;
    function Get( const aSymbol : AnsiString ) : Pointer;
  private
    constructor Create( const AName : AnsiString; aHandle: TLibHandle );
  private
    FHandle : TLibHandle;
    FName   : AnsiString;
    FRaise  : Boolean;
  public
    property Name : AnsiString read FName;
  end;
  {$WARNINGS ON}

implementation

uses vos;

constructor TLibrary.Create( const aName: AnsiString; aHandle: TLibHandle );
begin
  inherited Create;
  FName   := aName;
  FHandle := aHandle;
  FRaise  := False;
end;

class function TLibrary.Load( const aName : AnsiString; aErrorReport : boolean ) : TLibrary;
var iHandle  : TLibHandle;
    iName    : AnsiString;
begin
  iName := aName;
  {$IFDEF UNIX}
    {$IFDEF Darwin}
    if (Pos('.dylib',aName) = 0) and (Pos('/System/',aName) = 0) then
    begin
      {$IFDEF OSX_APP_BUNDLE}
      iName := GetOSXBundleFrameworksPath()+aName;
      {$ELSE}
      iName := GetOSXFrameworksPath()+aName;
      {$ENDIF}
    end;
    {$ENDIF}
    iHandle := TLibHandle( dlopen( PChar(iName), RTLD_LAZY or RTLD_GLOBAL) );
  {$ELSE}
    iHandle := LoadLibrary( iName );
  {$ENDIF}
  if iHandle = NilHandle then
  begin
    if aErrorReport
      then raise ELibraryError.Create( 'Can''t load library "' +iName+ '"' {$IFDEF UNIX} + ': ' + dlerror {$endif} )
      else Exit( nil );
  end
  else
    Load := Self.Create( aName, iHandle );
  Load.FRaise := aErrorReport;
end;

function TLibrary.Get( const aSymbol : AnsiString ) : Pointer;
begin
  Get := GetProcedureAddress( FHandle, PChar( aSymbol ) );
  if (Get = nil) and FRaise then ELibraryError.Create('Symbol "'+aSymbol+'" not found in library "'+FName+'"!');
end;

destructor TLibrary.Destroy;
begin
  UnloadLibrary( FHandle );
  inherited Destroy;
end;

end.

