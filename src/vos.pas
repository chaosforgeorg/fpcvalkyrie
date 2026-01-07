unit vos;
{$mode objfpc}
interface

uses Classes, SysUtils;

function GetResourcesPath() : AnsiString;

function GetOSXFrameworksPath() : AnsiString;
function GetOSXBundleFrameworksPath() : AnsiString;
function GetOSXBundlePath() : AnsiString;
procedure OpenWebPage(const URL : AnsiString);
procedure DisableAccessibilityShortcuts;
procedure RestoreAccessibilityShortcuts;

implementation

{$IFDEF WINDOWS}
uses Windows;

const
  // StickyKeys flags
  SKF_HOTKEYACTIVE    = $00000004;
  SKF_CONFIRMHOTKEY   = $00000008;

  // ToggleKeys flags 
  TKF_HOTKEYACTIVE    = $00000004;
  TKF_CONFIRMHOTKEY   = $00000008;

  // FilterKeys flags 
  FKF_HOTKEYACTIVE    = $00000004;
  FKF_CONFIRMHOTKEY   = $00000008;

  // SystemParametersInfo actions 
  SPI_GETSTICKYKEYS   = $003A;
  SPI_SETSTICKYKEYS   = $003B;
  SPI_GETTOGGLEKEYS   = $0034;
  SPI_SETTOGGLEKEYS   = $0035;
  SPI_GETFILTERKEYS   = $0032;
  SPI_SETFILTERKEYS   = $0033;

type
  TStickyKeys = record
    cbSize  : DWORD;
    dwFlags : DWORD;
  end;

  TToggleKeys = record
    cbSize  : DWORD;
    dwFlags : DWORD;
  end;

  TFilterKeys = record
    cbSize         : DWORD;
    dwFlags        : DWORD;
    iWaitMSec      : DWORD;
    iDelayMSec     : DWORD;
    iRepeatMSec    : DWORD;
    iBounceMSec    : DWORD;
  end;

var
  GOriginalStickyKeys : TStickyKeys;
  GOriginalToggleKeys : TToggleKeys;
  GOriginalFilterKeys : TFilterKeys;
  GAccessibilitySettingsSaved : Boolean = False;
{$ENDIF}

procedure DisableAccessibilityShortcuts;
{$IFDEF WINDOWS}
var
  iStickyKeys : TStickyKeys;
  iToggleKeys : TToggleKeys;
  iFilterKeys : TFilterKeys;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  if GAccessibilitySettingsSaved then Exit;

  { Save and disable StickyKeys hotkey (Shift x5) }
  GOriginalStickyKeys.cbSize := SizeOf(TStickyKeys);
  if SystemParametersInfo(SPI_GETSTICKYKEYS, SizeOf(TStickyKeys), @GOriginalStickyKeys, 0) then
  begin
    iStickyKeys := GOriginalStickyKeys;
    iStickyKeys.dwFlags := iStickyKeys.dwFlags and not (SKF_HOTKEYACTIVE or SKF_CONFIRMHOTKEY);
    SystemParametersInfo(SPI_SETSTICKYKEYS, SizeOf(TStickyKeys), @iStickyKeys, 0);
  end;

  { Save and disable ToggleKeys hotkey (NumLock for 5 seconds) }
  GOriginalToggleKeys.cbSize := SizeOf(TToggleKeys);
  if SystemParametersInfo(SPI_GETTOGGLEKEYS, SizeOf(TToggleKeys), @GOriginalToggleKeys, 0) then
  begin
    iToggleKeys := GOriginalToggleKeys;
    iToggleKeys.dwFlags := iToggleKeys.dwFlags and not (TKF_HOTKEYACTIVE or TKF_CONFIRMHOTKEY);
    SystemParametersInfo(SPI_SETTOGGLEKEYS, SizeOf(TToggleKeys), @iToggleKeys, 0);
  end;

  { Save and disable FilterKeys hotkey (Right Shift for 8 seconds) }
  GOriginalFilterKeys.cbSize := SizeOf(TFilterKeys);
  if SystemParametersInfo(SPI_GETFILTERKEYS, SizeOf(TFilterKeys), @GOriginalFilterKeys, 0) then
  begin
    iFilterKeys := GOriginalFilterKeys;
    iFilterKeys.dwFlags := iFilterKeys.dwFlags and not (FKF_HOTKEYACTIVE or FKF_CONFIRMHOTKEY);
    SystemParametersInfo(SPI_SETFILTERKEYS, SizeOf(TFilterKeys), @iFilterKeys, 0);
  end;

  GAccessibilitySettingsSaved := True;
{$ENDIF}
end;

procedure RestoreAccessibilityShortcuts;
begin
{$IFDEF WINDOWS}
  if not GAccessibilitySettingsSaved then Exit;

  SystemParametersInfo(SPI_SETSTICKYKEYS, SizeOf(TStickyKeys), @GOriginalStickyKeys, 0);
  SystemParametersInfo(SPI_SETTOGGLEKEYS, SizeOf(TToggleKeys), @GOriginalToggleKeys, 0);
  SystemParametersInfo(SPI_SETFILTERKEYS, SizeOf(TFilterKeys), @GOriginalFilterKeys, 0);

  GAccessibilitySettingsSaved := False;
{$ENDIF}
end;

{$IFDEF Darwin}
uses
  MacOSAll;

const OSXBundleResourcesDirectory  = '/Contents/Resources/';
      OSXBundleFrameworksDirectory = '/Contents/Frameworks/';
      OSXFrameworksDirectory       = '/Library/Frameworks/';

var OSXBundlePath : AnsiString = '';
{$ENDIF}

function GetResourcesPath() : AnsiString;
begin
  Result := '';
{$IFDEF Darwin}
  Result := GetOSXBundlePath() + OSXBundleResourcesDirectory;
{$ENDIF}
{$IFDEF Win32}
  Result := ExtractFilePath(ParamStr(0));
{$ENDIF}
end;

function GetOSXFrameworksPath: AnsiString;
begin
  {$IFDEF Darwin}
  Result := OSXFrameworksDirectory;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function GetOSXBundleFrameworksPath: AnsiString;
begin
  {$IFDEF Darwin}
  Result := GetOSXBundlePath() + OSXBundleFrameworksDirectory;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function GetOSXBundlePath() : AnsiString;
{$IFDEF Darwin}
var
  iPathRef   : CFURLRef;
  iPathCFStr : CFStringRef;
  iPathStr   : ShortString;
{$ENDIF}
begin
{$IFDEF Darwin}
  if OSXBundlePath <> '' then Exit( OSXBundlePath );
  iPathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  iPathCFStr := CFURLCopyFileSystemPath(iPathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(iPathCFStr, @iPathStr, 255, CFStringGetSystemEncoding());
  CFRelease(iPathRef);
  CFRelease(iPathCFStr);

  OSXBundlePath := iPathStr;
  Result        := OSXBundlePath;
{$ELSE}
  Result := '';
{$ENDIF}
end;

procedure OpenWebPage(const URL : AnsiString);
begin
{$IFDEF WINDOWS}
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOW);
{$ENDIF}
end;

{$IFDEF WINDOWS}
finalization
  RestoreAccessibilityShortcuts;
{$ENDIF}

end.

