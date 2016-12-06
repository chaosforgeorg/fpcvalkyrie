type
  TFileBitmap = record
    StartOffset : QWord;
    EndOffset   : QWord;
    IsComplete  : Cardinal;
    BitmapSize  : Cardinal;
    BlockSize   : Cardinal;
    Reserved    : Cardinal;
  end;
  PFileBitmap = ^TFileBitmap;

  SFILE_ADDFILE_CALLBACK = procedure(pvUserData : Pointer; dwBytesWritten : Cardinal; dwTotalBytes : Cardinal; bFinalCall : Boolean); extdecl;
  SFILE_COMPACT_CALLBACK = procedure(pvUserData : Pointer; dwWorkType : Cardinal; BytesProcessed : QWord; TotalBytes : QWord); extdecl;

  SFILE_FIND_DATA = record
      cFileName    : array[0..1024-1] of Char;
      szPlainName  : PChar;
      dwHashIndex  : Cardinal;
      dwBlockIndex : Cardinal;
      dwFileSize   : Cardinal;
      dwFileFlags  : Cardinal;
      dwCompSize   : Cardinal;
      dwFileTimeLo : Cardinal;
      dwFileTimeHi : Cardinal;
      lcLocale     : Cardinal;
  end;
  PSFILE_FIND_DATA = ^SFILE_FIND_DATA;

  SFILE_CREATE_MPQ = record
      cbSize         : Cardinal;
      dwMpqVersion   : Cardinal;
      pvUserData     : Pointer;
      cbUserData     : Cardinal;
      dwStreamFlags  : Cardinal;
      dwFileFlags1   : Cardinal;
      dwFileFlags2   : Cardinal;
      dwAttrFlags    : Cardinal;
      dwSectorSize   : Cardinal;
      dwRawChunkSize : Cardinal;
      dwMaxFileCount : Cardinal;
  end;
  PSFILE_CREATE_MPQ = ^SFILE_CREATE_MPQ;
