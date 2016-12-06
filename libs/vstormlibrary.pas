unit vstormlibrary;
{$include ../src/valkyrie.inc}
{$PACKRECORDS C}
{$MACRO ON}
interface
uses Classes, SysUtils, Types, vlibrary;

const
{$IFDEF WINDOWS}
  StormDefaultPath = 'StormLib.dll';
{$ELSE}
  {$IFDEF DARWIN}
    StormDefaultPath = 'StormLib.dylib';
  {$ELSE}
  StormDefaultPath = 'StormLib.so';
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl}
{$ELSE}
  {$DEFINE extdecl := stdcall}
{$ENDIF}

{$include vstormconst.inc}
{$include vstormtypes.inc}

type PHandle = ^THandle;

var
  SFileGetLocale : function() : Cardinal; extdecl;
  SFileSetLocale : function( lcNewLocale : Cardinal ) : Cardinal; extdecl;

  SFileOpenArchive    : function(const szMpqName : PChar; dwPriority : Cardinal; dwFlags : Cardinal; phMpq : PHandle) : Boolean; extdecl;
  SFileCreateArchive  : function(const szMpqName : PChar; dwFlags : Cardinal; dwMaxFileCount : Cardinal; phMpq : PHandle) : Boolean; extdecl;

  SFileGetArchiveBitmap : function(hMpq : THandle; pBitmap : PFileBitmap; Length : Cardinal; LengthNeeded : PCardinal ) : Boolean; extdecl;
  SFileFlushArchive     : function(hMpq : THandle) : Boolean; extdecl;
  SFileCloseArchive     : function(hMpq : THandle) : Boolean; extdecl;
  SFileAddListFile      : function(hMpq : THandle; szListFile : PChar ) : Integer; extdecl;

  SFileSetCompactCallback : function(hMpq : THandle; CompactCB : SFILE_COMPACT_CALLBACK; pvData : PChar) : Boolean; extdecl;
  SFileCompactArchive     : function(hMpq : THandle; const szListFile : PChar; bReserved : Boolean) : Boolean; extdecl;

  SFileGetMaxFileCount : function(hMpq : THandle) : Cardinal; extdecl;
  SFileSetMaxFileCount : function(hMpq : THandle; dwMaxFileCount : Cardinal) : Boolean; extdecl;

  SFileGetAttributes        : function(hMpq : THandle) : Cardinal; extdecl;
  SFileSetAttributes        : function(hMpq : THandle; dwFlags : Cardinal) : Boolean; extdecl;
  SFileUpdateFileAttributes : function(hMpq : THandle; const szFileName : PChar ) : Boolean; extdecl;

  SFileOpenPatchArchive : function(hMpq : THandle; const szPatchMpqName : PChar; const szPatchPathPrefix : PChar; dwFlags : Cardinal) : Boolean; extdecl;
  SFileIsPatchedArchive : function(hMpq : THandle) : Boolean; extdecl;

  SFileOpenFileEx     : function(hMpq : THandle; const szFileName : PChar; dwSearchScope : Cardinal; phFile : PHandle) : Boolean; extdecl;
  SFileGetFileSize    : function(hFile : THandle; pdwFileSizeHigh : PCardinal) : Cardinal; extdecl;
  SFileSetFilePointer : function(hFile : THandle; lFilePos : LongInt; plFilePosHigh : PLongInt; dwMoveMethod : Cardinal) : Cardinal; extdecl;
  SFileReadFile       : function(hFile : THandle; lpBuffer : Pointer; dwToRead : Cardinal; pdwRead : PCardinal; lpOverlapped : Pointer) : Boolean; extdecl;
  SFileCloseFile      : function(hFile : THandle) : Boolean; extdecl;

  SFileHasFile     : function(hMpq : THandle; const szFileName : PChar ) : Boolean; extdecl;
  SFileGetFileName : function(hFile : THandle; szFileName : PChar ) : Boolean; extdecl;
  SFileGetFileInfo : function(hMpqFile : THandle; dwInfoType : Cardinal; pvFileInfo : Pointer; cbFileInfo : Cardinal; pcbLengthNeeded : PCardinal) : Boolean; extdecl;
  SFileExtractFile : function(hMpq : THandle; const szToExtract : PChar; const szExtracted : PChar; dwSearchScope : Cardinal) : Boolean; extdecl;

  SFileVerifyFile       : function(hMpq : THandle; const szFileName : PChar; dwFlags : Cardinal) : Cardinal; extdecl;
  SFileVerifyRawData    : function(hMpq : THandle; dwWhatToVerify : Cardinal; const szFileName : PChar) : Integer; extdecl;
  SFileVerifyArchive    : function(hMpq : THandle) : Cardinal; extdecl;

  SFileFindFirstFile : function(hMpq : THandle; const szMask : PChar; lpFindFileData : PSFILE_FIND_DATA; const szListFile : PChar ) : THandle; extdecl;
  SFileFindNextFile  : function(hFind : THandle; lpFindFileData : PSFILE_FIND_DATA) : Boolean; extdecl;
  SFileFindClose     : function(hFind : THandle) : Boolean; extdecl;

  SListFileFindFirstFile : function(hMpq : THandle; const szListFile : PChar; const szMask : PChar; lpFindFileData : PSFILE_FIND_DATA ) : THandle; extdecl;
  SListFileFindNextFile  : function(hFind : THandle; lpFindFileData : PSFILE_FIND_DATA ) : Boolean; extdecl;
  SListFileFindClose     : function(hFind : THandle) : Boolean; extdecl;

  SFileEnumLocales : function(hMpq : THandle; const szFileName : PChar; plcLocales : PCardinal; pdwMaxLocales : PCardinal; dwSearchScope : Cardinal) : Integer; extdecl;

  SFileCreateFile : function(hMpq : THandle; const szArchivedName : PChar; FileTime : QWord; dwFileSize, lcLocale, dwFlags : Cardinal; phFile : PHandle) : Boolean; extdecl;
  SFileWriteFile  : function(hFile : THandle;  const pvData : Pointer; dwSize : Cardinal; dwCompression : Cardinal) : Boolean; extdecl;
  SFileFinishFile : function(hFile : THandle) : Boolean; extdecl;

  SFileAddFileEx  : function(hMpq : THandle; const szFileName : PChar; const szArchivedName : PChar; dwFlags, dwCompression, dwCompressionNext : Cardinal) : Boolean; extdecl;
  SFileAddFile    : function(hMpq : THandle; const szFileName : PChar; const szArchivedName : PChar; dwFlags : Cardinal) : Boolean; extdecl;
  SFileAddWave    : function(hMpq : THandle; const szFileName : PChar; const szArchivedName : PChar; dwFlags, dwQuality : Cardinal) : Boolean; extdecl;
  SFileRemoveFile : function(hMpq : THandle; const szFileName : PChar; dwSearchScope : Cardinal) : Boolean; extdecl;
  SFileRenameFile : function(hMpq : THandle; const szOldFileName : PChar; const szNewFileName : PChar) : Boolean; extdecl;
  SFileSetFileLocale : function(hFile : THandle; lcNewLocale : Cardinal) : Boolean; extdecl;
  SFileSetDataCompression : function(DataCompression : Cardinal) : Boolean; extdecl;

  SFileSetAddFileCallback : function(hMpq : THandle; AddFileCB : SFILE_ADDFILE_CALLBACK; pvData : Pointer) : Boolean; extdecl;

  SCompImplode     : function (pbOutBuffer : PChar; pcbOutBuffer : PInteger; pbInBuffer : PChar; cbInBuffer : Integer) : Integer; extdecl;
  SCompExplode     : function (pbOutBuffer : PChar; pcbOutBuffer : PInteger; pbInBuffer : PChar; cbInBuffer : Integer) : Integer; extdecl;
  SCompCompress    : function (pbOutBuffer : PChar; pcbOutBuffer : PInteger; pbInBuffer : PChar; cbInBuffer : Integer; uCompressionMask : Cardinal; nCmpType, nCmpLevel : Integer) : Integer; extdecl;
  SCompDecompress  : function (pbOutBuffer : PChar; pcbOutBuffer : PInteger; pbInBuffer : PChar; cbInBuffer : Integer) : Integer; extdecl;

var
  Storm : TLibrary = nil;

function LoadStorm( const aPath : AnsiString = StormDefaultPath ) : Boolean;

implementation

function LoadStorm( const aPath : AnsiString = StormDefaultPath ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := Storm.Get( aSymbol );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'Storm : Symbol "'+aSymbol+'" not found!' );
  end;
begin
  if Storm <> nil then Exit( True );
  Storm := TLibrary.Load( aPath );
  if Storm = nil then Exit( False );

  Pointer(SFileGetLocale) := GetSymbol('SFileGetLocale');
  Pointer(SFileSetLocale) := GetSymbol('SFileSetLocale');

  Pointer(SFileOpenArchive) := GetSymbol('SFileOpenArchive');
  Pointer(SFileCreateArchive) := GetSymbol('SFileCreateArchive');

  Pointer(SFileGetArchiveBitmap) := GetSymbol('SFileGetArchiveBitmap');
  Pointer(SFileFlushArchive) := GetSymbol('SFileFlushArchive');
  Pointer(SFileCloseArchive) := GetSymbol('SFileCloseArchive');
  Pointer(SFileAddListFile) := GetSymbol('SFileAddListFile');

  Pointer(SFileSetCompactCallback) := GetSymbol('SFileSetCompactCallback');
  Pointer(SFileCompactArchive) := GetSymbol('SFileCompactArchive');

  Pointer(SFileGetMaxFileCount) := GetSymbol('SFileGetMaxFileCount');
  Pointer(SFileSetMaxFileCount) := GetSymbol('SFileSetMaxFileCount');

  Pointer(SFileGetAttributes) := GetSymbol('SFileGetAttributes');
  Pointer(SFileSetAttributes) := GetSymbol('SFileSetAttributes');
  Pointer(SFileUpdateFileAttributes) := GetSymbol('SFileUpdateFileAttributes');

  Pointer(SFileOpenPatchArchive) := GetSymbol('SFileOpenPatchArchive');
  Pointer(SFileIsPatchedArchive) := GetSymbol('SFileIsPatchedArchive');

  Pointer(SFileOpenFileEx) := GetSymbol('SFileOpenFileEx');
  Pointer(SFileGetFileSize) := GetSymbol('SFileGetFileSize');
  Pointer(SFileSetFilePointer) := GetSymbol('SFileSetFilePointer');
  Pointer(SFileReadFile) := GetSymbol('SFileReadFile');
  Pointer(SFileCloseFile) := GetSymbol('SFileCloseFile');

  Pointer(SFileHasFile) := GetSymbol('SFileHasFile');
  Pointer(SFileGetFileName) := GetSymbol('SFileGetFileName');
  Pointer(SFileGetFileInfo) := GetSymbol('SFileGetFileInfo');
  Pointer(SFileExtractFile) := GetSymbol('SFileExtractFile');

  Pointer(SFileVerifyFile) := GetSymbol('SFileVerifyFile');
  Pointer(SFileVerifyRawData) := GetSymbol('SFileVerifyRawData');
  Pointer(SFileVerifyArchive) := GetSymbol('SFileVerifyArchive');

  Pointer(SFileFindFirstFile) := GetSymbol('SFileFindFirstFile');
  Pointer(SFileFindNextFile) := GetSymbol('SFileFindNextFile');
  Pointer(SFileFindClose) := GetSymbol('SFileFindClose');

  Pointer(SListFileFindFirstFile) := GetSymbol('SListFileFindFirstFile');
  Pointer(SListFileFindNextFile) := GetSymbol('SListFileFindNextFile');
  Pointer(SListFileFindClose) := GetSymbol('SListFileFindClose');

  Pointer(SFileEnumLocales) := GetSymbol('SFileEnumLocales');

  Pointer(SFileCreateFile) := GetSymbol('SFileCreateFile');
  Pointer(SFileWriteFile) := GetSymbol('SFileWriteFile');
  Pointer(SFileFinishFile) := GetSymbol('SFileFinishFile');

  Pointer(SFileAddFileEx) := GetSymbol('SFileAddFileEx');
  Pointer(SFileAddFile) := GetSymbol('SFileAddFile');
  Pointer(SFileAddWave) := GetSymbol('SFileAddWave');
  Pointer(SFileRemoveFile) := GetSymbol('SFileRemoveFile');
  Pointer(SFileRenameFile) := GetSymbol('SFileRenameFile');
  Pointer(SFileSetFileLocale) := GetSymbol('SFileSetFileLocale');
  Pointer(SFileSetDataCompression) := GetSymbol('SFileSetDataCompression');

  Pointer(SFileSetAddFileCallback) := GetSymbol('SFileSetAddFileCallback');

  Pointer(SCompImplode) := GetSymbol('SCompImplode');
  Pointer(SCompExplode) := GetSymbol('SCompExplode');
  Pointer(SCompCompress) := GetSymbol('SCompCompress');
  Pointer(SCompDecompress) := GetSymbol('SCompDecompress');

  Exit( True );
end;

end.

