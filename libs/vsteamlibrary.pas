unit vsteamlibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, Types, vlibrary;

// based on Steam API 1.62

const
{$IFDEF WINDOWS}
  SteamDefaultPath = 'steam_api64.dll';
{$ELSE}
  {$IFDEF DARWIN}
    SteamDefaultPath = 'libsteam_api.dylib';
  {$ELSE}
  SteamDefaultPath = 'libsteam_api.so';
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl}
{$ELSE}
  {$DEFINE extdecl := stdcall}
{$ENDIF}

{$include vsteamconst.inc}
{$include vsteamtypes.inc}

var
  SteamAPI_InitFlat       : function ( const msg : PSteamErrMsg ) : Steam_ESteamAPIInitResult; extdecl;
  SteamAPI_Shutdown       : procedure; extdecl;
  SteamAPI_IsSteamRunning : function () : Boolean; extdecl;
  SteamAPI_RunCallbacks   : procedure; extdecl;

  SteamAPI_ISteamClient_CreateSteamPipe     : function( instancePtr : PtrInt ) : HSteamPipe; extdecl;
  SteamAPI_ISteamClient_ConnectToGlobalUser : function( instancePtr : PtrInt; ahsteampipe : HSteamPipe ) : HSteamUser; extdecl;
  SteamAPI_ISteamClient_GetISteamFriends    : function( instancePtr : PtrInt; aHSteamUser : HSteamUser; aHSteamPipe : HSteamPipe; const pchVersion : PChar ) : ISteamFriends; extdecl;
  SteamAPI_ISteamClient_GetISteamUserStats  : function( instancePtr : PtrInt; aHSteamUser : HSteamUser; aHSteamPipe : HSteamPipe; const pchVersion : PChar ) : ISteamUserStats; extdecl;
  SteamAPI_ISteamClient_GetISteamUtils      : function( instancePtr : PtrInt;                           aHSteamPipe : HSteamPipe; const pchVersion : PChar ) : ISteamUtils; extdecl;
  SteamAPI_ISteamClient_GetISteamUser       : function( instancePtr : PtrInt; aHSteamUser : HSteamUser; aHSteamPipe : HSteamPipe; const pchVersion : PChar ) : ISteamUser; extdecl;
  SteamAPI_ISteamClient_GetISteamUGC        : function( instancePtr : PtrInt; aHSteamUser : HSteamUser; aHSteamPipe : HSteamPipe; const pchVersion : PChar ) : ISteamUGC; extdecl;

  SteamAPI_ISteamUser_GetSteamID            : function( aSelf : ISteamUser ) : QWord; extdecl;

  SteamAPI_ISteamUtils_GetAppID             : function( aSelf : ISteamUtils ) : DWord; extdecl;
  SteamAPI_ISteamUtils_IsOverlayEnabled     : function( aSelf : ISteamUtils ) : Boolean; extdecl;
  SteamAPI_ISteamUtils_IsAPICallCompleted   : function( aSelf : ISteamUtils; aApiCall : TSteamAPICall; aFailed : PBoolean ) : Boolean; extdecl;
  SteamAPI_ISteamUtils_GetAPICallResult     : function( aSelf : ISteamUtils; aApiCall : TSteamAPICall; pCallback : Pointer; cubCallback : Integer; iCallbackExpected : Integer; aFailed : PBoolean ) : Boolean; extdecl;

  SteamAPI_ISteamUserStats_GetStatInt32        : function( aSelf : ISteamUserStats; const pchName : PChar; pData : PInteger ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_GetStatFloat        : function( aSelf : ISteamUserStats; const pchName : PChar; pData : PSingle ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_SetStatInt32        : function( aSelf : ISteamUserStats; const pchName : PChar; nData : Integer ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_SetStatFloat        : function( aSelf : ISteamUserStats; const pchName : PChar; fData : Single ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_UpdateAvgRateStat   : function( aSelf : ISteamUserStats; const pchName : PChar; flCountThisSession : Single; dSessionLength : Double ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_GetAchievement      : function( aSelf : ISteamUserStats; const pchName : PChar; pbAchieved : PBoolean ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_SetAchievement      : function( aSelf : ISteamUserStats; const pchName : PChar) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_ClearAchievement    : function( aSelf : ISteamUserStats; const pchName : PChar) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_StoreStats          : function( aSelf : ISteamUserStats ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_RequestGlobalStats  : function( aSelf : ISteamUserStats; nHistoryDays : Integer ) : TSteamAPICall; extdecl;
  SteamAPI_ISteamUserStats_GetGlobalStatInt64  : function( aSelf : ISteamUserStats; const pchStatName : PChar; pData : PInt64 ) : Boolean; extdecl;
  SteamAPI_ISteamUserStats_GetGlobalStatDouble : function( aSelf : ISteamUserStats; const pchStatName : PChar; pData : PDouble ) : Boolean; extdecl;

  SteamAPI_ISteamFriends_GetPersonaName               : function( aSelf : ISteamFriends ) : PChar; extdecl;
  SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage : procedure( aSelf : ISteamFriends; const pchURL : PChar; eMode : Steam_EActivateGameOverlayToWebPageMode ); extdecl;
  SteamAPI_ISteamFriends_ActivateGameOverlayToStore   : procedure( aSelf : ISteamFriends; nAppID : TSteamAppId; eFlag : Steam_EOverlayToStoreFlag ); extdecl;

  SteamAPI_ISteamUGC_CreateItem            : function( aSelf : ISteamUGC; aAppId : TSteamAppId; aFileType : Steam_EWorkshopFileType ) : TSteamAPICall;
  SteamAPI_ISteamUGC_GetNumSubscribedItems : function( aSelf : ISteamUGC; aIncludeLocallyDisabled : Boolean ) : DWord;
  SteamAPI_ISteamUGC_GetSubscribedItems    : function( aSelf : ISteamUGC; pvecPublishedFileID : PSteamItemId; cMaxEntries : DWord; aIncludeLocallyDisabled : Boolean ) : DWord;
  SteamAPI_ISteamUGC_GetItemState          : function( aSelf : ISteamUGC; nPublishedFileID : TSteamItemId ) : DWord;
  SteamAPI_ISteamUGC_DownloadItem          : function( aSelf : ISteamUGC; nPublishedFileID : TSteamItemId; aHighPriority : Boolean ) : Boolean;
  SteamAPI_ISteamUGC_GetItemInstallInfo    : function( aSelf : ISteamUGC; nPublishedFileID : TSteamItemId; punSizeOnDisk : PQWord; const pchFolder : PChar; cchFolderSize : DWord; punTimeStamp : PDWord ) : Boolean;
  SteamAPI_ISteamUGC_StartItemUpdate       : function( aSelf : ISteamUGC; aAppId : TSteamAppId; nPublishedFileID : TSteamItemId ) : TSteamUGCUpdateHandle;
  SteamAPI_ISteamUGC_SetItemContent        : function( aSelf : ISteamUGC; aHandle : TSteamUGCUpdateHandle; const pszContentFolder : PChar ) : Boolean;
  SteamAPI_ISteamUGC_SetItemPreview        : function( aSelf : ISteamUGC; aHandle : TSteamUGCUpdateHandle; const pszPreviewFile : PChar ) : Boolean;
  SteamAPI_ISteamUGC_SubmitItemUpdate      : function( aSelf : ISteamUGC; aHandle : TSteamUGCUpdateHandle; const pchChangeNote : PChar ) : TSteamAPICall;
  SteamAPI_ISteamUGC_GetItemUpdateProgress : function( aSelf : ISteamUGC; aHandle : TSteamUGCUpdateHandle; punBytesProcessed : PQWord; punBytesTotal : PQWord ) : Steam_EItemUpdateStatus;

  SteamClient : function(): Pointer; extdecl;

var
  Steam : TLibrary = nil;

function LoadSteam( const aPath : AnsiString = SteamDefaultPath ) : Boolean;

implementation


function LoadSteam( const aPath : AnsiString = SteamDefaultPath ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := Steam.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'Steam : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if Steam <> nil then Exit( True );
  Steam := TLibrary.Load( aPath );
  if Steam = nil then Exit( False );

  Pointer(SteamAPI_InitFlat)       := GetSymbol('SteamAPI_InitFlat');
  Pointer(SteamAPI_Shutdown)       := GetSymbol('SteamAPI_Shutdown');
  Pointer(SteamAPI_IsSteamRunning) := GetSymbol('SteamAPI_IsSteamRunning');
  Pointer(SteamAPI_RunCallbacks)   := GetSymbol('SteamAPI_RunCallbacks');

  Pointer(SteamAPI_ISteamClient_CreateSteamPipe)     := GetSymbol('SteamAPI_ISteamClient_CreateSteamPipe');
  Pointer(SteamAPI_ISteamClient_ConnectToGlobalUser) := GetSymbol('SteamAPI_ISteamClient_ConnectToGlobalUser');
  Pointer(SteamAPI_ISteamClient_GetISteamFriends)    := GetSymbol('SteamAPI_ISteamClient_GetISteamFriends');
  Pointer(SteamAPI_ISteamClient_GetISteamUserStats)  := GetSymbol('SteamAPI_ISteamClient_GetISteamUserStats');
  Pointer(SteamAPI_ISteamClient_GetISteamUtils)      := GetSymbol('SteamAPI_ISteamClient_GetISteamUtils');
  Pointer(SteamAPI_ISteamClient_GetISteamUser)       := GetSymbol('SteamAPI_ISteamClient_GetISteamUser');
  Pointer(SteamAPI_ISteamClient_GetISteamUGC)        := GetSymbol('SteamAPI_ISteamClient_GetISteamUGC');

  Pointer(SteamAPI_ISteamUser_GetSteamID) := GetSymbol('SteamAPI_ISteamUser_GetSteamID');

  Pointer(SteamAPI_ISteamUtils_GetAppID)           := GetSymbol('SteamAPI_ISteamUtils_GetAppID');
  Pointer(SteamAPI_ISteamUtils_IsOverlayEnabled)   := GetSymbol('SteamAPI_ISteamUtils_IsOverlayEnabled');
  Pointer(SteamAPI_ISteamUtils_IsAPICallCompleted) := GetSymbol('SteamAPI_ISteamUtils_IsAPICallCompleted');
  Pointer(SteamAPI_ISteamUtils_GetAPICallResult)   := GetSymbol('SteamAPI_ISteamUtils_GetAPICallResult');

  Pointer(SteamAPI_ISteamUserStats_GetStatInt32)        := GetSymbol('SteamAPI_ISteamUserStats_GetStatInt32');
  Pointer(SteamAPI_ISteamUserStats_GetStatFloat)        := GetSymbol('SteamAPI_ISteamUserStats_GetStatFloat');
  Pointer(SteamAPI_ISteamUserStats_SetStatInt32)        := GetSymbol('SteamAPI_ISteamUserStats_SetStatInt32');
  Pointer(SteamAPI_ISteamUserStats_SetStatFloat)        := GetSymbol('SteamAPI_ISteamUserStats_SetStatFloat');
  Pointer(SteamAPI_ISteamUserStats_UpdateAvgRateStat)   := GetSymbol('SteamAPI_ISteamUserStats_UpdateAvgRateStat');
  Pointer(SteamAPI_ISteamUserStats_GetAchievement)      := GetSymbol('SteamAPI_ISteamUserStats_GetAchievement');
  Pointer(SteamAPI_ISteamUserStats_SetAchievement)      := GetSymbol('SteamAPI_ISteamUserStats_SetAchievement');
  Pointer(SteamAPI_ISteamUserStats_ClearAchievement)    := GetSymbol('SteamAPI_ISteamUserStats_ClearAchievement');
  Pointer(SteamAPI_ISteamUserStats_StoreStats)          := GetSymbol('SteamAPI_ISteamUserStats_StoreStats');
  Pointer(SteamAPI_ISteamUserStats_RequestGlobalStats)  := GetSymbol('SteamAPI_ISteamUserStats_RequestGlobalStats');
  Pointer(SteamAPI_ISteamUserStats_GetGlobalStatInt64)  := GetSymbol('SteamAPI_ISteamUserStats_GetGlobalStatInt64');
  Pointer(SteamAPI_ISteamUserStats_GetGlobalStatDouble) := GetSymbol('SteamAPI_ISteamUserStats_GetGlobalStatDouble');

  Pointer(SteamAPI_ISteamFriends_GetPersonaName)               := GetSymbol('SteamAPI_ISteamFriends_GetPersonaName');
  Pointer(SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage) := GetSymbol('SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage');
  Pointer(SteamAPI_ISteamFriends_ActivateGameOverlayToStore)   := GetSymbol('SteamAPI_ISteamFriends_ActivateGameOverlayToStore');

  Pointer(SteamAPI_ISteamUGC_CreateItem)            := GetSymbol('SteamAPI_ISteamUGC_CreateItem');
  Pointer(SteamAPI_ISteamUGC_GetNumSubscribedItems) := GetSymbol('SteamAPI_ISteamUGC_GetNumSubscribedItems');
  Pointer(SteamAPI_ISteamUGC_GetSubscribedItems)    := GetSymbol('SteamAPI_ISteamUGC_GetSubscribedItems');
  Pointer(SteamAPI_ISteamUGC_GetItemState)          := GetSymbol('SteamAPI_ISteamUGC_GetItemState');
  Pointer(SteamAPI_ISteamUGC_DownloadItem)          := GetSymbol('SteamAPI_ISteamUGC_DownloadItem');
  Pointer(SteamAPI_ISteamUGC_GetItemInstallInfo)    := GetSymbol('SteamAPI_ISteamUGC_GetItemInstallInfo');
  Pointer(SteamAPI_ISteamUGC_StartItemUpdate)       := GetSymbol('SteamAPI_ISteamUGC_StartItemUpdate');
  Pointer(SteamAPI_ISteamUGC_SetItemContent)        := GetSymbol('SteamAPI_ISteamUGC_SetItemContent');
  Pointer(SteamAPI_ISteamUGC_SetItemPreview)        := GetSymbol('SteamAPI_ISteamUGC_SetItemPreview');
  Pointer(SteamAPI_ISteamUGC_SubmitItemUpdate)      := GetSymbol('SteamAPI_ISteamUGC_SubmitItemUpdate');
  Pointer(SteamAPI_ISteamUGC_GetItemUpdateProgress) := GetSymbol('SteamAPI_ISteamUGC_GetItemUpdateProgress');

  Pointer(SteamClient) := GetSymbol('SteamClient');

  Exit( True );
end;

end.

