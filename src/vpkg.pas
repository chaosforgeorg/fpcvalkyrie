{$INCLUDE valkyrie.inc}
unit vpkg;
interface

uses
  vnode, Classes, SysUtils, zstream, vdf, vgenerics, idea
  { add your units here };
  
type TVDataWriter = procedure (const aFileName : AnsiString; aFileType : DWord; aFlags : TVDFClumpFlags = []; const aPackageName : AnsiString = '') of Object;
type TVDWriters   = array[1..16] of TVDataWriter;

type

{ TVDataCreator }

TVDataCreator = class(TVObject)
    constructor Create( const aFileName : AnsiString );
    procedure SetKey( const aKey : TIDEAKey );
    procedure RegisterWriter( aID : DWord; aWriter: TVDataWriter );
    procedure Add( const aFileName : AnsiString; aFileType : DWord; aFlags : TVDFClumpFlags = []; const aPackageName : AnsiString = '');

    // public only for binding
    procedure  AddLuaFile( const aFileName : AnsiString; aFileType : DWord; aFlags : TVDFClumpFlags = []; const aPackageName : AnsiString = '');
    procedure  AddTextFile( const aFileName : AnsiString; aFileType : DWord; aFlags : TVDFClumpFlags = []; const aPackageName : AnsiString = '');
    procedure  AddRawFile( const aFileName : AnsiString; aFileType : DWord; aFlags : TVDFClumpFlags = []; const aPackageName : AnsiString = '');
    destructor Destroy; override;
  private
    procedure Add( const aDir, aMask : AnsiString; aFileType : DWord; aFlags : TVDFClumpFlags = []; const aPackageName : AnsiString = '');
    function FileSize(FileName : AnsiString) : DWord;
    procedure Flush;
    function FileToStream( aFileHead : TVDFClumpHeader; aStream : TStream ) : DWord;
    function IsTempFileName( const aFileName : AnsiString ) : Boolean;
    function TrueFileName( const aFileName : AnsiString ) : AnsiString;
  private
    FName      : AnsiString;
    FHeader    : TVDFHeader;
    FData      : TVDCHArray;
    FSize      : DWord;
    FEKey      : TIDEAKey;
    FFileMark  : ShortString;
    FWriterSet : set of Byte;
    FWriters   : TVDWriters;
end;

implementation

uses vutil, strutils, vlualibrary;

{ TVDataCreator }

constructor TVDataCreator.Create( const aFileName : AnsiString );
begin
  FHeader.Signature := VDF_SIGNATURE;
  FHeader.Version   := 0;
  FHeader.Files     := 0;
  FName             := aFileName;
  FSize             := 256;
  FFileMark         := '__compiled';
  FillByte( FWriters, SizeOf( FWriters ), 0 );
  FWriterSet := [];
  SetLength(FData,FSize);

  RegisterWriter( FILETYPE_LUA,   @AddLuaFile );
  RegisterWriter( FILETYPE_HELP,  @AddTextFile );
  RegisterWriter( FILETYPE_ASCII, @AddTextFile );
  RegisterWriter( FILETYPE_XML,   @AddRawFile );
  RegisterWriter( FILETYPE_MUSIC, @AddRawFile );
  RegisterWriter( FILETYPE_SOUND, @AddRawFile );
  RegisterWriter( FILETYPE_IMAGE, @AddRawFile );
  RegisterWriter( FILETYPE_FONT,  @AddRawFile );
end;

procedure TVDataCreator.SetKey(const aKey: TIDEAKey);
begin
  FEKey := aKey;
end;

procedure TVDataCreator.RegisterWriter( aID : DWord; aWriter: TVDataWriter );
begin
  Include( FWriterSet, aID );
  FWriters[aID] := aWriter;
end;

function TVDataCreator.FileSize(FileName : AnsiString) : DWord;
var TF : File of byte;
begin
  Assign(TF,FileName);
  Reset(TF);
  FileSize := System.FileSize(TF);
  Close(TF);
end;

function LuaStreamWriter(L : Plua_State; const p : Pointer; sz : LongWord; ud : Pointer) : Integer; cdecl;
begin
  TStream(ud).WriteBuffer( p^, sz );
  Exit( 0 );
end;


procedure TVDataCreator.AddLuaFile( const aFileName : AnsiString; aFileType : DWord;
  aFlags: TVDFClumpFlags; const aPackageName : AnsiString);
var iCompiled  : AnsiString;
    iError     : AnsiString;
    iStream    : TStream;
    iState     : Plua_State;
    iResult    : Integer;
begin
  Log('Lua - Compiling '+aFileName+'...');
  iCompiled := ExtractFileName(aFileName)+FFileMark;
  iState    := lua_open();
  iResult   := luaL_loadfile( iState, PChar(aFileName) );
  if iResult <> 0 then
  begin
    Writeln( lua_tostring( iState, -1 ) );
    Log( LOGERROR, lua_tostring( iState, -1 ) );
    lua_close( iState );
    Exit;
  end;
  iStream := TFileStream.Create( iCompiled, fmCreate );
  try
    lua_dump( iState, @LuaStreamWriter, iStream );
  finally
    FreeAndNil( iStream );
    lua_close( iState );
  end;

  AddRawFile(iCompiled,aFileType,aFlags,aPackageName);
  Log('Lua - Compiled.');
end;

type TStringArray = specialize TGArray< AnsiString >;

procedure TVDataCreator.AddTextFile( const aFileName : AnsiString; aFileType : DWord;
  aFlags: TVDFClumpFlags; const aPackageName: AnsiString );
var StringList : TStringArray;
    StringFile : TFileStream;
    Compiled,TS: string;
    Counter    : DWord;
    TF         : Text;
begin
  if not FileExists(aFileName) then raise EFOpenError.Create('File "'+aFileName+'" not found!');
  Log('Adding "'+aFileName+'"...');

  Compiled   := ExtractFileName(aFileName)+FFileMark;
  StringList := TStringArray.Create;

  Assign(TF,aFileName);
  Reset(TF);
  while not EOF(TF) do
  begin
    Readln(TF,TS);
    StringList.Push(TS);
  end;
  Close(TF);

  StringFile := TFileStream.Create(Compiled,fmCreate);
  StringFile.WriteDWord(StringList.Size);
  for Counter := 0 to StringList.Size-1 do
    StringFile.WriteAnsiString(StringList[Counter]);

  FreeAndNil(StringFile);
  FreeAndNil(StringList);

  AddRawFile(Compiled,aFileType,aFlags,aPackageName);
end;

procedure TVDataCreator.AddRawFile( const aFileName: AnsiString; aFileType : DWord; aFlags: TVDFClumpFlags; const aPackageName : AnsiString = '');
begin
  if not FileExists(aFileName) then raise EFOpenError.Create('File "'+aFileName+'" not found!');
  Log('Adding "'+aFileName+'"...');

  if FHeader.Files+2 >= FSize then
  begin
    FSize := 2*FSize;
    SetLength(FData,FSize);
  end;
  
  with FData[FHeader.Files] do
  begin
    Name  := aFileName;
    Dir   := aPackageName;
    Size  := FileSize(aFileName);
    Pos   := 0;
    FType := aFileType;
    Flags := aFlags;
  end;

  Inc(FHeader.Files);
end;

procedure TVDataCreator.Add( const aDir,aMask: AnsiString; aFileType: DWord; aFlags: TVDFClumpFlags;
  const aPackageName: AnsiString);
var iSearchRec : TSearchRec;
    iDirectory : AnsiString;
    iWriter    : TVDataWriter;
begin
  iDirectory := aDir;
  if not aFileType in FWriterSet then
    raise EFOpenError.Create('Filetype #'+IntToStr(aFileType)+' not registered!');
  iWriter    := FWriters[aFileType];
  if iDirectory <> '' then iDirectory += PathDelim;
  Log('Adding "'+ iDirectory + aMask+'"...');
  if FindFirst(iDirectory + aMask,faAnyFile,iSearchRec) = 0 then
  repeat
    if iSearchRec.Name[1] = '.' then Continue;
    iWriter(iDirectory + iSearchRec.Name, aFileType, aFlags, aPackageName );
  until (FindNext(iSearchRec) <> 0);
  FindClose(iSearchRec);
end;

procedure TVDataCreator.Add( const aFileName : AnsiString; aFileType : DWord; aFlags : TVDFClumpFlags; const aPackageName : AnsiString );
var iFileName : AnsiString;
begin
  iFileName := aFileName;
  DoDirSeparators( iFileName );
  Add( ExtractFilePath( iFileName ), ExtractFileName( iFileName ), aFileType, aFlags, aPackageName );
end;

procedure TVDataCreator.Flush;
var iCount  : DWord;
    iCPos   : DWord;
    iStream : TStream;
const HEAD = SizeOf(TVDFHeader);
      CLPH = SizeOf(TVDFClumpHeader);
begin
  iCPos := HEAD;
  iCPos += FHeader.Files*CLPH;

  iStream := TFileStream.Create(FName,fmCreate);
  iStream.Write(FHeader,HEAD);
  Log('Header written - position = '+IntToStr(iStream.Position)+' = '+IntToStr(HEAD));
  for iCount := 0 to FHeader.Files - 1 do
    iStream.Write(FData[iCount],CLPH);
  Log('Chunk headers written - position = '+IntToStr(iStream.Position)+' = '+IntToStr(iCPos));
  for iCount := 0 to FHeader.Files - 1 do
  begin
    Log('Writing '+FData[iCount].Name+' - position = '+IntToStr(iStream.Position)+' = '+IntToStr(iCPos));
    FData[iCount].Pos := iCPos;
    iCPos += FileToStream( FData[iCount], iStream );
    if IsTempFileName( FData[iCount].Name ) then
    begin
      DeleteFile( FData[iCount].Name );
      FData[iCount].Name := ExtractFileName(TrueFileName(FData[iCount].Name));
    end
    else
      FData[iCount].Name := ExtractFileName(FData[iCount].Name);
    Log('Written '+FData[iCount].Name+' - position = '+IntToStr(iStream.Position)+' = '+IntToStr(iCPos));
  end;
  iStream.Seek(HEAD,soFromBeginning);
  for iCount := 0 to FHeader.Files - 1 do
    iStream.Write(FData[iCount],CLPH);
  FreeAndNil(iStream);
end;

destructor TVDataCreator.Destroy;
begin
  Flush;
  inherited Destroy;
end;

function TVDataCreator.FileToStream( aFileHead : TVDFClumpHeader; aStream : TStream ) : DWord;
var iFileStream : TStream;
    iFilter     : TStream;
    iFilter2    : TStream;
    iBefore     : DWord;
begin
  iBefore  := aStream.Position;
  iFilter  := nil;
  iFilter2 := nil;
  iFileStream := TFileStream.Create(aFileHead.Name,fmOpenRead);
  if (vdfEncrypted in aFileHead.Flags) and (vdfCompressed in aFileHead.Flags) then
  begin
    iFilter := iFileStream;
    iFilter2 := TIDEAEncryptStream.Create(FEKey,aStream);
    iFileStream := TCompressionStream.Create(clDefault,iFilter2,False);
    iFileStream.CopyFrom(iFilter,iFilter.Size);
  end
  else
  if vdfEncrypted in aFileHead.Flags then
  begin
    iFilter := iFileStream;
    iFileStream := TIDEAEncryptStream.Create(FEKey,aStream);
    iFileStream.CopyFrom(iFilter,iFilter.Size);
  end
  else
  if vdfCompressed in aFileHead.Flags then
  begin
    iFilter := iFileStream;
    iFileStream := TCompressionStream.Create(clDefault,aStream,False);
    iFileStream.CopyFrom(iFilter,iFilter.Size);
  end
  else
  begin
    aStream.CopyFrom(iFileStream,iFileStream.Size);
  end;
  FreeAndNil(iFilter);
  FreeAndNil(iFileStream);
  FreeAndNil(iFilter2);
  Exit(aStream.Position - iBefore);
end;

function TVDataCreator.IsTempFileName( const aFileName : AnsiString ) : Boolean;
begin
  Exit( RightStr(aFileName,Length(FFileMark)) = FFileMark );
end;

function TVDataCreator.TrueFileName( const aFileName : AnsiString ) : AnsiString;
begin
  if IsTempFileName( aFileName ) then
    Exit(LeftStr(aFileName,Length(aFileName)-Length(FFileMark)))
  else Exit(aFileName);
end;

end.

