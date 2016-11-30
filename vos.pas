unit vos;
{$mode objfpc}
interface

uses Classes, SysUtils;

function GetResourcesPath() : AnsiString;

function GetOSXFrameworksPath() : AnsiString;
function GetOSXBundleFrameworksPath() : AnsiString;
function GetOSXBundlePath() : AnsiString;
procedure OpenWebPage(const URL : AnsiString);

implementation

{$IFDEF Win32}
uses
  Windows;
{$ENDIF}
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
{$IFDEF Win32}
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOW);
{$ENDIF}
end;

end.

