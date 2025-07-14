// based on Steam API 1.62
type
  HSteamUser = LongInt;
  HSteamPipe = LongInt;
  TSteamAPICall = QWord;
  TSteamAppId   = DWord;
  TSteamItemId  = QWord;
  PSteamItemId  = ^QWord;
  TSteamUGCUpdateHandle = QWord;

  ISteamClient    = Pointer;
  ISteamUser      = Pointer;
  ISteamFriends   = Pointer;
  ISteamUserStats = Pointer;
  ISteamUGC       = Pointer;
  ISteamUtils     = Pointer;

const k_cchMaxSteamErrMsg = 1024;
type  TSteamErrMsg        = array[0..k_cchMaxSteamErrMsg - 1] of AnsiChar;
      PSteamErrMsg        = ^TSteamErrMsg;

type TSteamCallbackMsg = record
  unCallback : Cardinal;
  pubParam   : Pointer;
  cubParam   : Cardinal;
end;
type PSteamCallbackMsg = ^TSteamCallbackMsg;

type TSteamGamepadDismissed = record
  bSubmitted      : ByteBool;
  unSubmittedText : Cardinal;
end;
type PSteamGamepadDismissed = ^TSteamGamepadDismissed;
const STEAM_GAMEPAD_DISMISSED_CALLBACK = STEAM_UTILS_CALLBACKS + 14;


type Steam_ESteamAPIInitResult = (
  k_ESteamAPIInitResult_OK = 0,
  k_ESteamAPIInitResult_FailedGeneric = 1,
  k_ESteamAPIInitResult_NoSteamClient = 2,
  k_ESteamAPIInitResult_VersionMismatch = 3
);

type Steam_EResult = (
    k_EResultNone = 0,
    k_EResultOK = 1,
    k_EResultFail = 2,
    k_EResultNoConnection = 3,
    k_EResultInvalidPassword = 5,
    k_EResultLoggedInElsewhere = 6,
    k_EResultInvalidProtocolVer = 7,
    k_EResultInvalidParam = 8,
    k_EResultFileNotFound = 9,
    k_EResultBusy = 10,
    k_EResultInvalidState = 11,
    k_EResultInvalidName = 12,
    k_EResultInvalidEmail = 13,
    k_EResultDuplicateName = 14,
    k_EResultAccessDenied = 15,
    k_EResultTimeout = 16,
    k_EResultBanned = 17,
    k_EResultAccountNotFound = 18,
    k_EResultInvalidSteamID = 19,
    k_EResultServiceUnavailable = 20,
    k_EResultNotLoggedOn = 21,
    k_EResultPending = 22,
    k_EResultEncryptionFailure = 23,
    k_EResultInsufficientPrivilege = 24,
    k_EResultLimitExceeded = 25,
    k_EResultRevoked = 26,
    k_EResultExpired = 27,
    k_EResultAlreadyRedeemed = 28,
    k_EResultDuplicateRequest = 29,
    k_EResultAlreadyOwned = 30,
    k_EResultIPNotFound = 31,
    k_EResultPersistFailed = 32,
    k_EResultLockingFailed = 33,
    k_EResultLogonSessionReplaced = 34,
    k_EResultConnectFailed = 35,
    k_EResultHandshakeFailed = 36,
    k_EResultIOFailure = 37,
    k_EResultRemoteDisconnect = 38,
    k_EResultShoppingCartNotFound = 39,
    k_EResultBlocked = 40,
    k_EResultIgnored = 41,
    k_EResultNoMatch = 42,
    k_EResultAccountDisabled = 43,
    k_EResultServiceReadOnly = 44,
    k_EResultAccountNotFeatured = 45,
    k_EResultAdministratorOK = 46,
    k_EResultContentVersion = 47,
    k_EResultTryAnotherCM = 48,
    k_EResultPasswordRequiredToKickSession = 49,
    k_EResultAlreadyLoggedInElsewhere = 50,
    k_EResultSuspended = 51,
    k_EResultCancelled = 52,
    k_EResultDataCorruption = 53,
    k_EResultDiskFull = 54,
    k_EResultRemoteCallFailed = 55,
    k_EResultPasswordUnset = 56,
    k_EResultExternalAccountUnlinked = 57,
    k_EResultPSNTicketInvalid = 58,
    k_EResultExternalAccountAlreadyLinked = 59,
    k_EResultRemoteFileConflict = 60,
    k_EResultIllegalPassword = 61,
    k_EResultSameAsPreviousValue = 62,
    k_EResultAccountLogonDenied = 63,
    k_EResultCannotUseOldPassword = 64,
    k_EResultInvalidLoginAuthCode = 65,
    k_EResultAccountLogonDeniedNoMail = 66,
    k_EResultHardwareNotCapableOfIPT = 67,
    k_EResultIPTInitError = 68,
    k_EResultParentalControlRestricted = 69,
    k_EResultFacebookQueryError = 70,
    k_EResultExpiredLoginAuthCode = 71,
    k_EResultIPLoginRestrictionFailed = 72,
    k_EResultAccountLockedDown = 73,
    k_EResultAccountLogonDeniedVerifiedEmailRequired = 74,
    k_EResultNoMatchingURL = 75,
    k_EResultBadResponse = 76,
    k_EResultRequirePasswordReEntry = 77,
    k_EResultValueOutOfRange = 78,
    k_EResultUnexpectedError = 79,
    k_EResultDisabled = 80,
    k_EResultInvalidCEGSubmission = 81,
    k_EResultRestrictedDevice = 82,
    k_EResultRegionLocked = 83,
    k_EResultRateLimitExceeded = 84,
    k_EResultAccountLoginDeniedNeedTwoFactor = 85,
    k_EResultItemDeleted = 86,
    k_EResultAccountLoginDeniedThrottle = 87,
    k_EResultTwoFactorCodeMismatch = 88,
    k_EResultTwoFactorActivationCodeMismatch = 89,
    k_EResultAccountAssociatedToMultiplePartners = 90,
    k_EResultNotModified = 91,
    k_EResultNoMobileDevice = 92,
    k_EResultTimeNotSynced = 93,
    k_EResultSmsCodeFailed = 94,
    k_EResultAccountLimitExceeded = 95,
    k_EResultAccountActivityLimitExceeded = 96,
    k_EResultPhoneActivityLimitExceeded = 97,
    k_EResultRefundToWallet = 98,
    k_EResultEmailSendFailure = 99,
    k_EResultNotSettled = 100,
    k_EResultNeedCaptcha = 101,
    k_EResultGSLTDenied = 102,
    k_EResultGSOwnerDenied = 103,
    k_EResultInvalidItemType = 104,
    k_EResultIPBanned = 105,
    k_EResultGSLTExpired = 106,
    k_EResultInsufficientFunds = 107,
    k_EResultTooManyPending = 108,
    k_EResultNoSiteLicensesFound = 109,
    k_EResultWGNetworkSendExceeded = 110,
    k_EResultAccountNotFriends = 111,
    k_EResultLimitedUserAccount = 112,
    k_EResultCantRemoveItem = 113,
    k_EResultAccountDeleted = 114,
    k_EResultExistingUserCancelledLicense = 115,
    k_EResultCommunityCooldown = 116,
    k_EResultNoLauncherSpecified = 117,
    k_EResultMustAgreeToSSA = 118,
    k_EResultLauncherMigrated = 119,
    k_EResultSteamRealmMismatch = 120,
    k_EResultInvalidSignature = 121,
    k_EResultParseFailure = 122,
    k_EResultNoVerifiedPhone = 123,
    k_EResultInsufficientBattery = 124,
    k_EResultChargerRequired = 125,
    k_EResultCachedCredentialInvalid = 126,
    K_EResultPhoneNumberIsVOIP = 127
  );

type Steam_EActivateGameOverlayToWebPageMode = (
    k_EActivateGameOverlayToWebPageMode_Default = 0,
    k_EActivateGameOverlayToWebPageMode_Modal = 1
);

type Steam_EOverlayToStoreFlag = (
    k_EOverlayToStoreFlag_None = 0,
    k_EOverlayToStoreFlag_AddToCart = 1,
    k_EOverlayToStoreFlag_AddToCartAndShow = 2
);

type Steam_EItemUpdateStatus = (
    k_EItemUpdateStatusInvalid              = 0,
    k_EItemUpdateStatusPreparingConfig      = 1,
    k_EItemUpdateStatusPreparingContent     = 2,
    k_EItemUpdateStatusUploadingContent     = 3,
    k_EItemUpdateStatusUploadingPreviewFile = 4,
    k_EItemUpdateStatusCommittingChanges    = 5
);

type Steam_EWorkshopFileType = (
    k_EWorkshopFileTypeCommunity        = 0,
    k_EWorkshopFileTypeMicrotransaction = 1,
    k_EWorkshopFileTypeCollection       = 2,
    k_EWorkshopFileTypeArt              = 3,
    k_EWorkshopFileTypeVideo            = 4,
    k_EWorkshopFileTypeScreenshot       = 5,
    k_EWorkshopFileTypeGame             = 6,
    k_EWorkshopFileTypeSoftware         = 7,
    k_EWorkshopFileTypeConcept          = 8,
    k_EWorkshopFileTypeWebGuide         = 9,
    k_EWorkshopFileTypeIntegratedGuide  = 10,
    k_EWorkshopFileTypeMerch            = 11,
    k_EWorkshopFileTypeControllerBinding      = 12,
    k_EWorkshopFileTypeSteamworksAccessInvite = 13,
    k_EWorkshopFileTypeSteamVideo       = 14,
    k_EWorkshopFileTypeGameManagedItem  = 15,
    k_EWorkshopFileTypeClip             = 16
);

type Steam_EGamepadTextInputMode = (
    k_EGamepadTextInputModeNormal = 0,
    k_EGamepadTextInputModePassword = 1
);

type Steam_EGamepadTextInputLineMode = (
    k_EGamepadTextInputLineModeSingleLine = 0,
    k_EGamepadTextInputLineModeMultipleLines = 1
);

const STEAM_CREATE_ITEM_RESULT_CALLBACK = STEAM_UGC_CALLBACKS + 3;
type TSteamCreateItemResult = record
    // CALLBACK = 3403
    Result          : Steam_EResult;
    PublishedFileId : TSteamItemId;
    AcceptAgreement : Boolean;
  end;

const STEAM_SUBMIT_ITEM_UPDATE_RESULT_CALLBACK = STEAM_UGC_CALLBACKS + 4;
type TSteamSubmitItemUpdateResult = record
    // CALLBACK = 3404
    Result          : Steam_EResult;
    AcceptAgreement : Boolean;
    PublishedFileId : TSteamItemId;
  end;

const STEAM_GLOBAL_STATS_RECEIVED_RESULT_CALLBACK = STEAM_USERSTATS_CALLBACKS + 12;
type TSteamGlobalStatsReceivedResult = record
    // CALLBACK = 1112
    GameId : QWord;
    Result : Steam_EResult;
  end;


