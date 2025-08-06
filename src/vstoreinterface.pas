{$include valkyrie.inc}
unit vstoreinterface;
interface
uses vnode, vgenerics;

type TStoreType = ( StoreNone, StoreSteam );

type TModInfo = class
  private
    FFolder : Ansistring;
    FID     : QWord;
  public
    constructor Create( aFolder : Ansistring; aID : QWord );
  public
    property Folder : Ansistring read FFolder;
    property ID     : QWord      read FID;
  end;

type TModArray = specialize TGObjectArray< TModInfo >;

type TStoreInterface = class( TVObject )
  class function Get : TStoreInterface;
  function GetUserId : QWord; virtual;
  function IsInitialized : Boolean; virtual;
  function IsOverlayEnabled : Boolean; virtual;
  function IsSteamDeck : Boolean; virtual;
  function IsSteam : Boolean; virtual;
  function GetUsername : Ansistring; virtual;
  procedure Update; virtual;
  function SetAchievement( const aID : Ansistring ) : Boolean; virtual;
  function IncStat( const aID : Ansistring ) : Boolean; virtual;
  function MarkStat( const aID : Ansistring ) : Boolean; virtual;
  function GetGlobalStat( const aID : Ansistring ) : Int64; virtual;
  function FlushStatistics : Boolean; virtual;
  function OpenDLCPage( aAppID : DWord ) : Boolean; virtual;
  function OpenStorePage( aAppID : DWord ) : Boolean; virtual;
  function GetStoreType : TStoreType; virtual;
  function ModPublish( const aPath, aModID : Ansistring ) : QWord; virtual;
  function ModUpdate( const aPath : Ansistring; aModID : QWord ) : Boolean; virtual;
  function GetMods : TModArray; virtual;
  function StartText( const aPrompt : Ansistring; aMaxLength : Integer; const aCurrent : AnsiString = '' ) : Boolean; virtual;
  function GetText( var aPrompt : Ansistring; aCancel : PBoolean = nil ) : Boolean; virtual;
  destructor Destroy; override;
protected
  constructor Create;
end;


implementation

uses sysutils, vsteam, vdebug;

var GStoreInterface : TStoreInterface = nil;

constructor TModInfo.Create( aFolder : Ansistring; aID : QWord );
begin
  FFolder := aFolder;
  FID     := aId;
end;

class function TStoreInterface.Get : TStoreInterface;
begin
  if Assigned ( GStoreInterface ) then Exit( GStoreInterface );
  if TSteam.TryLoadLibrary then
    GStoreInterface := TSteam.Create
  else
  begin
    vdebug.Log('TStoreInterface : no store API detected, initializing stub');
    GStoreInterface := TStoreInterface.Create;
  end;
  Exit( GStoreInterface );
end;

function TStoreInterface.GetUserId : QWord;
begin
  Exit( 0 );
end;

function TStoreInterface.IsInitialized : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.IsSteamDeck : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.IsSteam : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.IsOverlayEnabled : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.GetUsername : Ansistring;
begin
  Exit( '' );
end;

procedure TStoreInterface.Update;
begin
end;

function TStoreInterface.SetAchievement( const aID : Ansistring ) : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.IncStat( const aID : Ansistring ) : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.MarkStat( const aID : Ansistring ) : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.GetGlobalStat( const aID : Ansistring ) : Int64;
begin
  Exit( 0 );
end;

function TStoreInterface.FlushStatistics : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.OpenDLCPage( aAppID : DWord ) : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.OpenStorePage( aAppID : DWord ) : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.GetStoreType : TStoreType;
begin
  Exit( StoreNone );
end;

function TStoreInterface.ModPublish( const aPath, aModID : Ansistring ) : QWord;
begin
  Exit( 0 );
end;

function TStoreInterface.ModUpdate( const aPath : Ansistring; aModID : QWord ) : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.GetMods : TModArray;
begin
  Exit( nil );
end;

function TStoreInterface.StartText( const aPrompt : Ansistring; aMaxLength : Integer; const aCurrent : AnsiString = '' ) : Boolean;
begin
  Exit( False );
end;

function TStoreInterface.GetText( var aPrompt : Ansistring; aCancel : PBoolean = nil ) : Boolean;
begin
  Exit( False );
end;

destructor TStoreInterface.Destroy;
begin
  inherited Destroy;
end;

constructor TStoreInterface.Create;
begin
  inherited Create;
end;

finalization

FreeAndNil( GStoreInterface );

end.

