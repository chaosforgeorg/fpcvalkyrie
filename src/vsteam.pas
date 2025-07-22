{$include valkyrie.inc}
unit vsteam;
interface
uses vnode, vutil, vstoreinterface;

type  TSteam = class( TStoreInterface )
  constructor Create;

  function GetUserId : QWord; override;
  function IsInitialized : Boolean; override;
  function IsOverlayEnabled : Boolean; override;
  function IsSteamDeck : Boolean; override;
  function GetUsername : Ansistring; override;
  procedure Update; override;
  function SetAchievement( const aID : Ansistring ) : Boolean; override;
  function IncStat( const aID : Ansistring ) : Boolean; override;
  function GetGlobalStat( const aID : Ansistring ) : Int64; override;
  function FlushStatistics : Boolean; override;
  function OpenDLCPage( aAppID : DWord ) : Boolean; override;
  function OpenStorePage( aAppID : DWord ) : Boolean; override;
  function GetStoreType : TStoreType; override;
  function ModPublish( const aPath, aModID : Ansistring ) : QWord; override;
  function ModUpdate( const aPath : Ansistring; aModID : QWord ) : Boolean; override;
  function GetMods : TModArray; override;
  function StartText( const aPrompt : Ansistring; aMaxLength : Integer; const aCurrent : AnsiString = '' ) : Boolean; override;
  function GetText( var aPrompt : Ansistring; aCancel : PBoolean = nil ) : Boolean; override;
  destructor Destroy; override;
  class function TryLoadLibrary : Boolean;
private
  function LoadGlobalStats : Boolean;
private
  FUserName   : Ansistring;
  FGlobals    : Boolean;
  FAppId      : DWord;
  FUserId     : QWord;
  FMods       : TModArray;
  FText       : AnsiString;
  FTextReady  : Boolean;
  FTextCancel : Boolean;
end;


implementation

uses vsteamlibrary, sysutils;

type TSteamFriends = class( TVObject )
    constructor Create( aFriends : ISteamFriends );
    function GetPersonaName() : Ansistring;
    procedure ActivateOverlayToStore( aAppId : TSteamAppId );
    procedure ActivateOverlayWebpage( const aPath : Ansistring );
  private
    FFriends : ISteamFriends;
end;

constructor TSteamFriends.Create( aFriends : ISteamFriends );
begin
  inherited Create;
  FFriends := aFriends;
end;

function TSteamFriends.GetPersonaName() : Ansistring;
begin
  Result := SteamAPI_ISteamFriends_GetPersonaName( FFriends );
end;

procedure TSteamFriends.ActivateOverlayToStore( aAppId : TSteamAppId );
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayToStore( FFriends, aAppId, k_EOverlayToStoreFlag_None );
end;

procedure TSteamFriends.ActivateOverlayWebpage( const aPath : Ansistring );
begin
  SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage( FFriends, PChar( aPath ), k_EActivateGameOverlayToWebPageMode_Default );
end;

type TSteamUser = class( TVObject )
    constructor Create( aUser : ISteamUser );
    function getSteamId : QWord;
  private
    FUser : ISteamUser;
end;
 
constructor TSteamUser.Create( aUser : ISteamUser );
begin
  inherited Create;
  FUser := aUser;
end;

function TSteamUser.getSteamId : QWord;
begin
  Exit( SteamAPI_ISteamUser_GetSteamID( FUser ) );
end;

type TSteamUtils = class( TVObject )
    constructor Create( aUtils : ISteamUtils );
    function GetAppId : TSteamAppId;
    function IsOverlayEnabled : Boolean;
    function IsApiCallCompleted( aApiCall : TSteamAPICall ) : Boolean;
    function GetApiCallResult( aApiCall : TSteamAPICall; aData : Pointer; aDataSize, aCallbackNum : Integer; aFailed : PBoolean ) : Boolean;
    function ShowGamepadTextInput( eInputMode : Steam_EGamepadTextInputMode; eLineInputMode : Steam_EGamepadTextInputLineMode; const pchDescription : PChar; unCharMax : DWord; const pchExistingText : PChar ) : Boolean;
    function GetEnteredGamepadTextLength : Integer;
    function GetEnteredGamepadTextInput( pchText : PChar; cchText : DWord ) : Boolean;
    function DismissGamepadTextInput : Boolean;
    function IsRunningOnSteamDeck : Boolean;
  private
    FUtils : ISteamUtils;
end;

function TSteamUtils.GetAppId : TSteamAppId;
begin
  Exit( SteamAPI_ISteamUtils_GetAppID( FUtils ) );
end;

function TSteamUtils.IsOverlayEnabled : Boolean;
begin
  Exit( SteamAPI_ISteamUtils_IsOverlayEnabled( FUtils ) );
end;

function TSteamUtils.IsApiCallCompleted( aApiCall : TSteamAPICall ) : Boolean;
begin
  Exit( SteamAPI_ISteamUtils_IsAPICallCompleted( FUtils, aApiCall, nil ) );
end;

function TSteamUtils.GetApiCallResult( aApiCall : TSteamAPICall; aData : Pointer; aDataSize, aCallbackNum : Integer; aFailed : PBoolean ) : Boolean;
begin
  Exit( SteamAPI_ISteamUtils_GetAPICallResult( FUtils, aApiCall, aData, aDataSize, aCallbackNum, aFailed ) );
end;

function TSteamUtils.ShowGamepadTextInput( eInputMode : Steam_EGamepadTextInputMode; eLineInputMode : Steam_EGamepadTextInputLineMode; const pchDescription : PChar; unCharMax : DWord; const pchExistingText : PChar ) : Boolean;
begin
  Exit( SteamAPI_ISteamUtils_ShowGamepadTextInput( FUtils, eInputMode, eLineInputMode, pchDescription, unCharMax, pchExistingText ) );
end;

function TSteamUtils.GetEnteredGamepadTextLength : Integer;
begin
  Exit( SteamAPI_ISteamUtils_GetEnteredGamepadTextLength( FUtils ) );
end;

function TSteamUtils.GetEnteredGamepadTextInput( pchText : PChar; cchText : DWord ) : Boolean;
begin
  Exit( SteamAPI_ISteamUtils_GetEnteredGamepadTextInput( FUtils, pchText, cchText ) );
end;

function TSteamUtils.DismissGamepadTextInput : Boolean;
begin
  Exit( SteamAPI_ISteamUtils_DismissGamepadTextInput( FUtils ) );
end;

function TSteamUtils.IsRunningOnSteamDeck : Boolean;
begin
  Exit( SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck( FUtils ) );
end;

constructor TSteamUtils.Create( aUtils : ISteamUtils );
begin
  inherited Create;
  FUtils := aUtils;
end;

type TSteamUserStats = class( TVObject )
    constructor Create( aUserStats : ISteamUserStats );
    function GetStat( const aName : Ansistring; out aValue : LongInt ) : Boolean;
    function GetStat( const aName : Ansistring; out aValue : Single ) : Boolean;
    function SetStat( const aName : Ansistring; aValue : LongInt ) : Boolean;
    function SetStat( const aName : Ansistring; aValue : Single ) : Boolean;
    function RequestGlobalStats( aDays : Integer = 0 ) : TSteamAPICall;
    function GetGlobalStat( const aName : Ansistring; out aValue : Int64 ) : Boolean;
    function GetGlobalStat( const aName : Ansistring; out aValue : Double ) : Boolean;
    function SetAchievement( const aName : Ansistring ) : Boolean;
    function StoreStats : Boolean;
  private
    FUserStats : ISteamUserStats;
end;

constructor TSteamUserStats.Create( aUserStats : ISteamUserStats );
begin
  inherited Create;
  FUserStats := aUserStats;
end;

function TSteamUserStats.GetStat( const aName : Ansistring; out aValue : LongInt ) : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_GetStatInt32( FUserStats, PChar(aName), @aValue ) );
end;

function TSteamUserStats.GetStat( const aName : Ansistring; out aValue : Single ) : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_GetStatFloat( FUserStats, PChar(aName), @aValue ) );
end;

function TSteamUserStats.SetStat( const aName : Ansistring; aValue : LongInt ) : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_SetStatInt32( FUserStats, PChar(aName), aValue ) );
end;

function TSteamUserStats.SetStat( const aName : Ansistring; aValue : Single ) : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_SetStatFloat( FUserStats, PChar(aName), aValue ) );
end;

function TSteamUserStats.RequestGlobalStats( aDays : Integer = 0 ) : TSteamAPICall;
begin
  Exit( SteamAPI_ISteamUserStats_RequestGlobalStats( FUserStats, aDays ) );
end;

function TSteamUserStats.GetGlobalStat( const aName : Ansistring; out aValue : Int64 ) : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_GetGlobalStatInt64( FUserStats, PChar( aName ), @aValue ) );
end;

function TSteamUserStats.GetGlobalStat( const aName : Ansistring; out aValue : Double ) : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_GetGlobalStatDouble( FUserStats, PChar( aName ), @aValue ) );
end;

function TSteamUserStats.SetAchievement( const aName : Ansistring ) : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_SetAchievement( FUserStats, PChar( aName ) ) );
end;

function TSteamUserStats.StoreStats : Boolean;
begin
  Exit( SteamAPI_ISteamUserStats_StoreStats( FUserStats ) );
end;

type TSteamUGCInfo = record
  Path      : Ansistring;
  Size      : QWord;
  TimeStamp : DWord;
end;

type TSteamUGC = class( TVObject )
    constructor Create( aUGC : ISteamUGC );
    function CreateItem( aAppId : TSteamAppId ) : TSteamApiCall;
    function GetNumSubscribedItems : DWord;
    function GetSubscribedItems( aItems : PSteamItemId; aMaxItems : DWord ) : DWord;
    function GetItemState( aItem : TSteamItemId ) : DWord;
    function DownloadItem( aItem : TSteamItemId; aHighPriority : Boolean ) : Boolean;
    function GetItemInstallInfo( aItem : TSteamItemId; out aInfo : TSteamUGCInfo ) : Boolean;
    function StartItemUpdate( aAppId : TSteamAppId; aItem : TSteamItemId ) : TSteamUGCUpdateHandle;
    function SetItemContent( aHandle : TSteamUGCUpdateHandle; const aContentFolder : Ansistring ) : Boolean;
    function SetItemPreview( aHandle : TSteamUGCUpdateHandle;const aPreviewFile : Ansistring  ) : Boolean;
    function SubmitItemUpdate( aHandle : TSteamUGCUpdateHandle;const aChangeNote : Ansistring  ) : TSteamApiCall;
    function GetItemUpdateProgress( aHandle : TSteamUGCUpdateHandle; aBytesProcessed, aBytesTotal : PQWord ) : Steam_EItemUpdateStatus;
  private
    FUGC : ISteamUGC;
end;

constructor TSteamUGC.Create( aUGC : ISteamUGC );
begin
  inherited Create;
  FUGC := aUGC;
end;

function TSteamUGC.CreateItem( aAppId : TSteamAppId ) : TSteamApiCall;
begin
  Exit( SteamAPI_ISteamUGC_CreateItem( FUGC, aAppId, k_EWorkshopFileTypeCommunity ) );
end;

function TSteamUGC.GetNumSubscribedItems : DWord;
begin
  Exit( SteamAPI_ISteamUGC_GetNumSubscribedItems( FUGC, False ) );
end;

function TSteamUGC.GetSubscribedItems( aItems : PSteamItemId; aMaxItems : DWord ) : DWord;
begin
  Exit( SteamAPI_ISteamUGC_GetSubscribedItems( FUGC, aItems, aMaxItems, False ) );
end;

function TSteamUGC.GetItemState( aItem : TSteamItemId ) : DWord;
begin
  Exit( SteamAPI_ISteamUGC_GetItemState( FUGC, aItem ) );
end;

function TSteamUGC.DownloadItem( aItem : TSteamItemId; aHighPriority : Boolean ) : Boolean;
begin
  Exit( SteamAPI_ISteamUGC_DownloadItem( FUGC, aItem, aHighPriority ) );
end;

function TSteamUGC.GetItemInstallInfo( aItem : TSteamItemId; out aInfo : TSteamUGCInfo ) : Boolean;
var iBuffer : array[0..255] of Char;
begin
  Result := SteamAPI_ISteamUGC_GetItemInstallInfo( FUGC, aItem, @(aInfo.Size), iBuffer, 255, @(aInfo.TimeStamp) );
  aInfo.Path:= Ansistring( PChar( iBuffer ) );
end;

function TSteamUGC.StartItemUpdate( aAppId : TSteamAppId; aItem : TSteamItemId ) : TSteamUGCUpdateHandle;
begin
  Exit( SteamAPI_ISteamUGC_StartItemUpdate( FUGC, aAppId, aItem ) );
end;

function TSteamUGC.SetItemContent( aHandle : TSteamUGCUpdateHandle; const aContentFolder : Ansistring ) : Boolean;
begin
  Exit( SteamAPI_ISteamUGC_SetItemContent( FUGC, aHandle, PChar( aContentFolder ) ) );
end;

function TSteamUGC.SetItemPreview( aHandle : TSteamUGCUpdateHandle;const aPreviewFile : Ansistring  ) : Boolean;
begin
  Exit( SteamAPI_ISteamUGC_SetItemPreview( FUGC, aHandle, PChar( aPreviewFile ) ) );
end;

function TSteamUGC.SubmitItemUpdate( aHandle : TSteamUGCUpdateHandle;const aChangeNote : Ansistring  ) : TSteamApiCall;
begin
  Exit( SteamAPI_ISteamUGC_SubmitItemUpdate( FUGC, aHandle, PChar( aChangeNote ) ) );
end;

function TSteamUGC.GetItemUpdateProgress( aHandle : TSteamUGCUpdateHandle; aBytesProcessed, aBytesTotal : PQWord ) : Steam_EItemUpdateStatus;
begin
  Exit( SteamAPI_ISteamUGC_GetItemUpdateProgress( FUGC, aHandle, aBytesProcessed, aBytesTotal ) );
end;

type TSteamManualDispatch = class( TVObject )
    constructor Create( aPipe : HSteamPipe );
    procedure Update;
    function GetNextCallback( aCallback : PSteamCallbackMsg ) : Boolean;
    procedure FreeLastCallback;
    function GetApiCallResult( aApiCall : TSteamAPICall; aCallback : Pointer; acCallback : Integer; aCallbackExpected : Integer; aFailed : PBoolean ) : Boolean;
  private
    FPipe : HSteamPipe;
end;

constructor TSteamManualDispatch.Create( aPipe : HSteamPipe );
begin
  inherited Create;
  FPipe := aPipe;
  SteamAPI_ManualDispatch_Init;
end;

procedure TSteamManualDispatch.Update;
begin
  SteamAPI_ManualDispatch_RunFrame( FPipe );
end;

function TSteamManualDispatch.GetNextCallback( aCallback : PSteamCallbackMsg ) : Boolean;
begin
  Exit( SteamAPI_ManualDispatch_GetNextCallback( FPipe, aCallback ) );
end;

procedure TSteamManualDispatch.FreeLastCallback;
begin
  SteamAPI_ManualDispatch_FreeLastCallback( FPipe );
end;

function TSteamManualDispatch.GetApiCallResult( aApiCall : TSteamAPICall; aCallback : Pointer; acCallback : Integer; aCallbackExpected : Integer; aFailed : PBoolean ) : Boolean;
begin
  Exit( SteamAPI_ManualDispatch_GetAPICallResult( FPipe, aApiCall, aCallback, acCallback, aCallbackExpected, aFailed ) );
end;

type TSteamClient = class( TVObject )
    function IsInitialized : Boolean;
    constructor Create;
    destructor Destroy; override;
  private
    FClient     : ISteamClient;
    FUserStats  : TSteamUserStats;
    FFriends    : TSteamFriends;
    FUser       : TSteamUser;
    FUGC        : TSteamUGC;
    FUtils      : TSteamUtils;
    FDispatch   : TSteamManualDispatch;
    FClientPipe : HSteamPipe;
    FUserPipe   : HSteamUser;
  public
    property UserStats : TSteamUserStats      read FUserStats;
    property Friends   : TSteamFriends        read FFriends;
    property User      : TSteamUser           read FUser;
    property UGC       : TSteamUGC            read FUGC;
    property Utils     : TSteamUtils          read FUtils;
    property Dispatch  : TSteamManualDispatch read FDispatch;
  end;

type  TSteamCore = class( TVObject )
  public
    class function TryLoadLibrary : Boolean;
    class function GetClient : TSteamClient;
    class function GetCore : TSteamCore;

    procedure RunCallbacks;
    function IsInitialized: Boolean;

    constructor Create;
    destructor Destroy; override;
  private
    FInitialized : Boolean;
  end;

{ TSteamClient }

constructor TSteamClient.Create;
var iFriends   : ISteamFriends;
    iUserStats : ISteamUserStats;
    iUser      : ISteamUser;
    iUGC       : ISteamUGC;
    iUtils     : ISteamUtils;
begin
  inherited Create;
  if not TSteamCore.GetCore().IsInitialized() then Exit;

  FClient := SteamClient();
  if not Assigned( FClient ) then
  begin
    Log( LOGERROR, 'Failed to initialize client!' );
    Exit;
  end;

  FClientPipe := SteamAPI_ISteamClient_CreateSteamPipe( PtrInt( FClient ) );
  if FClientPipe = 0 then
  begin
    Log( LOGERROR, 'Failed to create steam pipe!' );
    FClient := nil;
    Exit;
  end;

  FUserPipe := SteamAPI_ISteamClient_ConnectToGlobalUser( PtrInt( FClient ), FClientPipe );
  if FUserPipe = 0 then
  begin
    Log( LOGERROR, 'Failed to create user pipe!' );
    FClient := nil;
    Exit;
  end;

  FFriends := nil;
  iFriends := SteamAPI_ISteamClient_GetISteamFriends( PtrInt( FClient ), FUserPipe, FClientPipe, STEAMFRIENDS_INTERFACE_VERSION );
  if Assigned( iFriends ) then
    FFriends := TSteamFriends.Create( iFriends )
  else
    Log( LOGWARN, 'GetISteamFriends failed!' );

  FUser := nil;
  iUser := SteamAPI_ISteamClient_GetISteamUser( PtrInt( FClient ), FUserPipe, FClientPipe, STEAMUSER_INTERFACE_VERSION );
  if Assigned( iUser ) then
    FUser := TSteamUser.Create( iUser )
  else
    Log( LOGWARN, 'GetISteamUser failed!' );

  FUtils := nil;
  iUtils := SteamAPI_ISteamClient_GetISteamUtils( PtrInt( FClient ), FClientPipe, STEAMUTILS_INTERFACE_VERSION );
  if Assigned( iUtils ) then
    FUtils := TSteamUtils.Create( iUtils )
  else
    Log( LOGWARN, 'GetISteamUtils failed!' );

  FUserStats := nil;
  iUserStats := SteamAPI_ISteamClient_GetISteamUserStats( PtrInt( FClient ), FUserPipe, FClientPipe, STEAMUSERSTATS_INTERFACE_VERSION );
  if Assigned( iUserStats ) then
    FUserStats := TSteamUserStats.Create( iUserStats )
  else
    Log( LOGWARN, 'GetISteamUserStats failed!' );

  FUGC := nil;
  iUGC := SteamAPI_ISteamClient_GetISteamUGC( PtrInt( FClient ), FUserPipe, FClientPipe, STEAMUGC_INTERFACE_VERSION );
  if Assigned( iUGC ) then
    FUGC := TSteamUGC.Create( iUGC )
  else
    Log( LOGWARN, 'GetISteamUGC failed!' );

  FDispatch := TSteamManualDispatch.Create( FClientPipe );
  Log( 'Initialized.' );
end;

destructor TSteamClient.Destroy;
begin
  inherited Destroy;
end;

function TSteamClient.IsInitialized: Boolean;
begin
  Result := FClient <> nil;
end;

var GSteamCore   : TSteamCore = nil;
    GSteamClient : TSteamClient = nil;

{ TSteamCore }

constructor TSteamCore.Create;
var iMsg : TSteamErrMsg;
begin
  inherited Create;
  if not LoadSteam then
    Exit;

  FInitialized := ( SteamAPI_InitFlat( @iMsg ) = k_ESteamAPIInitResult_OK );
  if not FInitialized then
  begin
    Log( LOGWARN, 'SteamAPI failed to initialize - not ran from Steam, or no steamappid? (%s)',[PChar(@iMsg)]);
    Exit;
  end;

  Log('SteamAPI initialized');
end;

destructor TSteamCore.Destroy;
begin
  if FInitialized then
  begin
    SteamAPI_Shutdown();
    Log('SteamAPI deinitialized');
  end;
  inherited Destroy;
end;

procedure TSteamCore.RunCallbacks;
begin
  if FInitialized then
    SteamAPI_RunCallbacks();
end;

function TSteamCore.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

class function TSteamCore.TryLoadLibrary: Boolean;
begin
  Result := LoadSteam();
end;

class function TSteamCore.GetCore: TSteamCore;
begin
  if not Assigned(GSteamCore) then
    GSteamCore := TSteamCore.Create;
  Result := GSteamCore;
end;

class function TSteamCore.GetClient: TSteamClient;
begin
  GetCore;
  if not Assigned(GSteamClient) then
    GSteamClient := TSteamClient.Create;
  Result := GSteamClient;
end;


constructor TSteam.Create;
var iClient       : TSteamClient;
    iUGCMods, i   : DWord;
    iMods         : array[0..255] of QWord;
    iDownloading  : Boolean;
    iTimer        : QWord;
    iInfo         : TSteamUGCInfo;
begin
  inherited Create;
  Log('initializing...');
  FGlobals    := False;
  FText       := '';
  FTextReady  := False;
  FTextCancel := False;

  iClient := TSteamCore.GetClient;
  if not iClient.IsInitialized then Exit;

  FUserName := '';
  if iClient.Friends <> nil then
  begin
    FUserName := iClient.Friends.GetPersonaName();
    if FUserName <> '' then
      Log( 'Hello, '+FUserName+'!' )
    else
      Log( LOGWARN, 'GetPersonaName failed!' );
  end;

  if iClient.User <> nil then
  begin
    FUserId := iClient.User.GetSteamId;
    if FUserId = 0 then
      Log( LOGWARN, 'GetSteamId failed!' );
  end;

  if iClient.Utils <> nil then
  begin
    FAppId := iClient.Utils.GetAppId;
    if FAppId = 0 then
      Log( LOGWARN, 'GetAppId failed!' );
  end;

  if iClient.UGC <> nil then
  begin
    iUGCMods := iClient.UGC.GetNumSubscribedItems;
    Log( 'UGC mod count - %d', [iUGCMods] );
    if iUGCMods > 0 then
    begin
      FMods := TModArray.Create;;
      if iUGCMods > 255 then
      begin
        Log( LOGERROR, 'TOO MANY INSTALLED MODS!' );
        iUGCMods := 255;
      end;
      if iClient.UGC.GetSubscribedItems( iMods, iUGCMods ) > 0 then
      begin
        // TODO : >=8? Unhack this
        for i := 0 to iUGCMods-1 do
          if iClient.UGC.GetItemState( iMods[i] ) >= 8 then
            iClient.UGC.DownloadItem( iMods[i], True );

        iDownloading := false;
        iTimer := GetTickCount64;
        repeat
          iDownloading := false;
          TSteamCore.GetCore.RunCallbacks;
          for i := 0 to iUGCMods-1 do
            if iClient.UGC.GetItemState( iMods[i] ) >= 8 then
              iDownloading := True;
        until ( not iDownloading ) or ( ( GetTickCount64 - iTimer ) > 10000 );

        for i := 0 to iUGCMods-1 do
          if iClient.UGC.GetItemInstallInfo( iMods[i], iInfo ) then
          begin
            Log( 'mod %d identified in %s', [iMods[i], iInfo.Path ] );
            FMods.Push( TModInfo.Create( iInfo.Path, iMods[i] ) );
          end;
      end
      else
        Log( LOGWARN, 'GetSubscribedItems failed!' );
    end;
  end;
end;

function TSteam.GetUserId : QWord;
begin
  Exit( FUserId );
end;

function TSteam.IsInitialized : Boolean;
begin
  Exit( TSteamCore.GetClient.IsInitialized );
end;

function TSteam.IsOverlayEnabled : Boolean;
begin
  if ( not IsInitialized ) or ( TSteamCore.GetClient.Utils = nil ) then Exit( False );
  Exit( TSteamCore.GetClient.Utils.IsOverlayEnabled );
end;

function TSteam.IsSteamDeck : Boolean;
var iClient : TSteamClient;
begin
  if ( not IsInitialized ) then Exit( False );
  iClient := TSteamCore.GetClient;
  if not Assigned( iClient ) then Exit( False );
  if not Assigned( iClient.Utils ) then Exit( False );
  Exit( iClient.Utils.IsRunningOnSteamDeck );
end;

function TSteam.GetUsername : Ansistring;
begin
  Exit( FUserName );
end;

procedure TSteam.Update;
var iDispatch : TSteamManualDispatch;
    iMsg      : TSteamCallbackMsg;
    iBuf      : array[0..255] of AnsiChar;
    iLength   : Integer;
begin
  if ( not IsInitialized ) then Exit;
  iDispatch := TSteamCore.GetClient.Dispatch;
  iDispatch.Update;
  while iDispatch.GetNextCallback(@iMsg) do
  begin
    case iMsg.unCallback of
      STEAM_GAMEPAD_DISMISSED_CALLBACK :
        begin
          FTextReady  := True;
          FTextCancel := not PSteamGamepadDismissed(iMsg.pubParam)^.bSubmitted;
          FText       := '';
          if Assigned( TSteamCore.GetClient.Utils ) then
          begin
            iLength := TSteamCore.GetClient.Utils.GetEnteredGamepadTextLength;
            if iLength < Length(iBuf) then
              if TSteamCore.GetClient.Utils.GetEnteredGamepadTextInput( @iBuf[0], iLength ) then
                FText := UTF8ToString(iBuf);
          end;
        end;
    end;
    iDispatch.FreeLastCallback;
  end;
end;

function TSteam.SetAchievement( const aID : Ansistring ) : Boolean;
var iUserStats : TSteamUserStats;
begin
  if ( not IsInitialized ) then Exit( False );
  iUserStats := TSteamCore.GetClient.UserStats;
  if iUserStats = nil then Exit( False );
  if not iUserStats.SetAchievement( aID ) then
  begin
    Log( LOGWARN, 'SetAchievement('+aID+') failed!' );
    Exit( False );
  end;
  if not iUserStats.StoreStats then
  begin
    Log( LOGWARN, 'StoreStats failed!' );
    Exit( False );
  end;
  Exit( True );
end;

function TSteam.IncStat( const aID : Ansistring ) : Boolean;
var iValue : LongInt;
begin
  if ( not IsInitialized ) or ( TSteamCore.GetClient.UserStats = nil ) then Exit( False );
  if not TSteamCore.GetClient.UserStats.GetStat( aID, iValue ) then
  begin
    Log( LOGWARN, 'GetStat('+aID+') failed!' );
    Exit( False );
  end;
  if not TSteamCore.GetClient.UserStats.SetStat( aID, LongInt( iValue + 1 ) ) then
  begin
    Log( LOGWARN, 'SetStat('+aID+') failed!' );
    Exit( False );
  end;
  Exit( True );
end;

function TSteam.GetGlobalStat( const aID : Ansistring ) : Int64;
begin
  if ( not IsInitialized ) or ( TSteamCore.GetClient.UserStats = nil ) then Exit( 0 );

  if not FGlobals then
  begin
    LoadGlobalStats;
    FGlobals := True;
  end;

  if not TSteamCore.GetClient.UserStats.GetGlobalStat( aID, Result ) then
  begin
    Log( LOGWARN, 'GetGlobalStat('+aID+') failed!' );
    Exit( 0 );
  end;
end;

function TSteam.FlushStatistics : Boolean;
begin
  if ( not IsInitialized ) or ( TSteamCore.GetClient.UserStats = nil ) then Exit( False );
  if not TSteamCore.GetClient.UserStats.StoreStats then
  begin
    Log( LOGWARN, 'StoreStats failed!' );
    Exit( False );
  end;
  Exit( True );
end;

function TSteam.OpenDLCPage( aAppID : DWord ) : Boolean;
begin
  if ( not IsInitialized ) or ( TSteamCore.GetClient.Friends = nil ) then Exit( False );
  if aAppID = 0 then aAppID := FAppID;
  TSteamCore.GetClient.Friends.ActivateOverlayToStore( aAppId );
  Exit( True );
end;

function TSteam.OpenStorePage( aAppID : DWord ) : Boolean;
begin
  if ( not IsInitialized ) or ( TSteamCore.GetClient.Friends = nil ) then Exit( False );
  if aAppID = 0 then aAppID := FAppID;
  TSteamCore.GetClient.Friends.ActivateOverlayWebpage( 'https://store.steampowered.com/app/' + IntToStr( aAppId ) + '/' );
end;

function TSteam.GetStoreType : TStoreType;
begin
  Exit( StoreSteam );
end;

function TSteam.ModPublish( const aPath, aModID : Ansistring ) : QWord;
var iUtils     : TSteamUtils;
    iUGC       : TSteamUGC;
    iCall      : TSteamAPICall;
    iFail      : Boolean;
    iResult    : TSteamCreateItemResult;
    iItemID    : TSteamItemId;
    iPath      : Ansistring;
    iText      : Text;
begin
  if ( not IsInitialized ) then Exit( 0 );
  iUGC   := TSteamCore.GetClient.UGC;
  iUtils := TSteamCore.GetClient.Utils;
  if ( iUGC = nil ) or ( iUtils = nil ) then Exit( 0 );

  iCall := iUGC.CreateItem( FAppId );
  if iCall = 0 then
  begin
    Log( LOGERROR, 'CreateItem failed!' );
    Exit( 0 );
  end;

  while true do
  begin
    TSteamCore.GetCore.RunCallbacks;
    if not iUtils.IsApiCallCompleted( iCall ) then
      Continue;

    if iUtils.GetApiCallResult( iCall,
      @iResult, sizeof( iResult ),
      STEAM_CREATE_ITEM_RESULT_CALLBACK,
      @iFail ) then
      Break;
  end;

  if iFail then
  begin
    Log( LOGERROR, 'CreateItem - timed out!' );
    Exit( 0 );
  end;

  if iResult.Result <> k_EResultOK  then
  begin
    Log( LOGERROR, 'CreateItem - failed, error code %d!', [Integer( iResult.Result )] );
    Exit( 0 );
  end;

  iItemID := iResult.PublishedFileId;
  Log( 'CreateItem - assigned workshop id %d!', [iItemID] );
  iPath := aPath + '/meta.lua';
  Assign( iText, iPath );
  {$I-}
  Rewrite( iText );
  {$I+}
  if IOResult <> 0 then
  begin
    Log( LOGERROR, 'can''t create file : '+iPath+'!');
    Exit( 0 );
  end;

  Writeln( iText, 'meta = {' );
  Writeln( iText, '  id            = "' + aModID + '",');
  Writeln( iText, '  save_version  = 100,' );
  Writeln( iText, '  save_agnostic = false,' );
  Writeln( iText, '  workshop_id   = '+IntToStr( iItemID )+ ',' );
  Writeln( iText, '}' );
  Close( iText );
  Log('mod published successfully : '+ iPath );
  Exit( iItemID );
end;

function TSteam.ModUpdate( const aPath : Ansistring; aModID : QWord ) : Boolean;
var iUtils        : TSteamUtils;
    iUGC          : TSteamUGC;
    iCall         : TSteamAPICall;
    iFail         : Boolean;
    iResult       : TSteamSubmitItemUpdateResult;
    iHandle       : TSteamUGCUpdateHandle;
    iAbsPath      : AnsiString;
    iTimer, iTT   : QWord;
    iStep, iStage : Integer;
    iTot, iProc   : QWord;
begin
  if ( not IsInitialized ) then Exit( False );
  iUGC   := TSteamCore.GetClient.UGC;
  iUtils := TSteamCore.GetClient.Utils;
  if ( iUGC = nil ) or ( iUtils = nil ) then Exit( False );

  iAbsPath := GetCurrentDir + '/' + aPath;
  Log( 'Absolute path : '+ iAbsPath );

  iHandle := iUGC.StartItemUpdate( FAppId, aModID );
  iUGC.SetItemContent( iHandle, iAbsPath );

  if FileExists( iAbsPath + '.png' ) then
  begin
    Log( 'Thumbnail found : ' + iAbsPath + '.png' );
    iUGC.SetItemPreview( iHandle, iAbsPath + '.png' );
  end;

  iCall := iUGC.SubmitItemUpdate( iHandle, 'update' );
  if iCall = 0 then
  begin
    Log( LOGERROR, 'SubmitItemiTTUpdate failed!' );
    Exit( False );
  end;

  iTT    := 0;
  iTimer := GetTickCount64;
  iStage := 0;
  while true do
  begin
    TSteamCore.GetCore.RunCallbacks;

    if (iTimer mod 10) = 0 then
    begin
      iStep := Integer( iUGC.GetItemUpdateProgress( iHandle, @iProc, @iTot ) );
      if iStep <> iStage then
      begin
       case iStep of
          1: Log( 'preparing config...' );
          2: Log( 'preparing content...' );
          3: Log( 'uploading content...' );
          4: Log( 'uploading preview...' );
          5: Log( 'committing changes...' );
       end;
       iStage := iStep;
       iTT := 0;
      end;

      Inc( iTT );
      if iTot = 0  then
      begin
        if iTT > 50 then
        begin
          iTT -= 50;
          Log( '.' );
        end;
      end
      else
      begin
        if iTT > 10 then
        begin
          iTT -= 50;
          Log( 'progress %d/%d', [iProc, iTot] );
        end;
      end;
    end;

    if not iUtils.IsApiCallCompleted( iCall ) then
      Continue;

    if iUtils.GetApiCallResult( iCall,
      @iResult, sizeof( iResult ),
      STEAM_SUBMIT_ITEM_UPDATE_RESULT_CALLBACK,
      @iFail ) then
      Break;
  end;

  if iFail then
  begin
    Log( LOGERROR, 'SubmitItemUpdate - timed out!' );
    Exit( False );
  end;

  if iResult.Result <> k_EResultOK  then
  begin
    Log( LOGERROR, 'SubmitItemUpdate - failed, error code %d!', [Integer( iResult.Result )] );
    Exit( False );
  end;

  Log( 'SubmitItemUpdate - upload completed!' );
  Exit( True );
end;

function TSteam.GetMods : TModArray;
begin
  Exit( FMods );
end;

function TSteam.StartText( const aPrompt : Ansistring; aMaxLength : Integer; const aCurrent : AnsiString = '' ) : Boolean;
var iClient : TSteamClient;
begin
  if ( not IsInitialized ) then Exit( False );
  iClient := TSteamCore.GetClient;
  if not Assigned( iClient ) then Exit( False );
  if not Assigned( iClient.Utils ) then Exit( False );
  if not iClient.Utils.IsRunningOnSteamDeck then Exit( False );

  FText       := '';
  FTextReady  := False;
  FTextCancel := False;

  Exit( iClient.Utils.ShowGamepadTextInput( k_EGamepadTextInputModeNormal, k_EGamepadTextInputLineModeSingleLine, PChar( aPrompt ), DWord( aMaxLength ), PChar( aCurrent ) ) );
end;

function TSteam.GetText( var aPrompt : Ansistring; aCancel : PBoolean = nil ) : Boolean;
begin
  if FTextReady then
  begin
    aPrompt := FText;
    if Assigned( aCancel ) then aCancel^ := FTextCancel;
    FText       := '';
    FTextCancel := False;
    FTextReady  := False;
    Exit( True );
  end;
  Exit( False );
end;

class function TSteam.TryLoadLibrary : Boolean;
begin
  Exit( LoadSteam( SteamDefaultPath, False ) and ( TSteamCore.GetClient <> nil ) );
end;

destructor TSteam.Destroy;
begin
  FreeAndNil( FMods );
  FreeAndNil( GSteamClient );
  FreeAndNil( GSteamCore );
  inherited Destroy;
end;

function TSteam.LoadGlobalStats : Boolean;
var iUserstats : TSteamUserStats;
    iUtils     : TSteamUtils;
    iCall      : TSteamAPICall;
    iFail      : Boolean;
    iResult    : TSteamGlobalStatsReceivedResult;
begin
  if ( not IsInitialized ) then Exit( False );
  iUserstats := TSteamCore.GetClient.Userstats;
  iUtils     := TSteamCore.GetClient.Utils;
  if ( iUserStats = nil ) or ( iUtils = nil ) then Exit( False );

  iCall := iUserstats.RequestGlobalStats( 0 );

  if iCall = 0 then
  begin
    Log( LOGERROR, 'LoadGlobalStats failed!' );
    Exit( False );
  end;

  while true do
  begin
    TSteamCore.GetCore.RunCallbacks;
    if not iUtils.IsApiCallCompleted( iCall ) then
      Continue;

    if iUtils.GetApiCallResult( iCall,
      @iResult, sizeof( iResult ),
      STEAM_GLOBAL_STATS_RECEIVED_RESULT_CALLBACK,
      @iFail ) then
      Break;
  end;

  if iFail then
  begin
    Log( LOGERROR, 'LoadGlobalStats - timed out!' );
    Exit( False );
  end;

  if iResult.Result <> k_EResultOK  then
  begin
    Log( LOGERROR, 'LoadGlobalStats - failed, error code %d!', [Integer( iResult.Result )] );
    Exit( False );
  end;

  Log( 'LoadGlobalStats - successful' );
  Exit( true );
end;

finalization

FreeAndNil( GSteamClient );
FreeAndNil( GSteamCore );

end.

