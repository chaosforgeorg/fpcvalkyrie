{$INCLUDE valkyrie.inc}
// @abstract(Data file classes for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-01-14 22:16:41 +0100 (Mon, 14 Jan 2008) $)
//
// Introduces data file handling classes, with compression and encryption.
//
//  @html <div class="license">
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or (at your
//  option) any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
//  for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this library; if not, write to the Free Software Foundation,
//  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//  @html </div>

unit vdf;
interface
uses vnode, Classes, SysUtils, zstream, vgenerics, idea, dom, xmlread;

// default types
const
  FILETYPE_RAW   = 1;
  FILETYPE_HELP  = 2;
  FILETYPE_XML   = 3;
  FILETYPE_ASCII = 4;
  FILETYPE_LUA   = 5;
  FILETYPE_MUSIC = 6;
  FILETYPE_SOUND = 7;
  FILETYPE_IMAGE = 8;
  FILETYPE_FONT  = 9;

type

TVDFClumpFlags = set of (vdfCompressed,vdfEncrypted);

TVDFHeader = packed record
  Signature : string[8];
  Version   : DWord;
  Files     : DWord;
end;

TVDFClumpHeader = packed record
  Size  : DWord;
  Pos   : DWord;
  Dir   : string[64];
  Name  : string[64];
  Flags : TVDFClumpFlags;
  FType : DWord;
end;

TVDCHArray = array of TVDFClumpHeader;

TVDFLoader = procedure(Stream : TStream; Name : AnsiString; Size : DWord) of Object;

TVDFLoaders = specialize TGArray<TVDFLoader>;

{ TVDataFile }

TVDataFile = class(TVObject)
  DKKey   : TIDEAKey;
  constructor Create(FileName : Ansistring);
  function GetXMLDocument(const FileName : Ansistring; DirName : Ansistring = '') : TXMLDocument;
  function GetFile(FileName : Ansistring; DirName : Ansistring = '') : TStream;
  function GetFileSize(FileName : Ansistring; DirName : Ansistring = '') : Int64;
  function FileExists(FileName : Ansistring; DirName : Ansistring = '') : Boolean;
  procedure Load(PackageID : AnsiString = '');
  procedure LoadFile(FileName : Ansistring; PackageID : AnsiString = '');
  procedure RegisterLoader(LoaderID : DWord; Loader : TVDFLoader);
  destructor Destroy; override;
  private
  procedure LoadFile( FileID : DWord );
  function GetFileID(FileName,Dir : Ansistring) : DWord; overload;
  private
  FStream  : TFileStream;
  FData    : TVDCHArray;
  FFiles   : DWord;
  FName    : Ansistring;
  FLoaders : TVDFLoaders;
end;

const VDF_SIGNATURE = 'VDFILE02';

implementation

uses vutil;


{ TVDataFile }

constructor TVDataFile.Create(FileName : Ansistring);
var Header : TVDFHeader;
    Count  : DWord;
begin
  FName := FileName;
  Log('Loading "'+FName+'"...');
  try
    FStream := TFileStream.Create(FileName,fmOpenRead);
  except
    raise Exception.Create('Can''t open Valkyrie Data File "'+FName+'"!');
  end;
  {$HINTS OFF}
  FStream.Read(Header,SizeOf(Header));
  {$HINTS ON}
  if Header .Signature<> VDF_SIGNATURE then raise Exception.Create('Corrupted Valkyrie Data File "'+FName+'"!');
  FFiles := Header.Files;
  if FFiles = 0 then Exit;
  SetLength(FData,FFiles);
  for Count := 0 to FFiles-1 do
    FStream.Read(FData[Count],SizeOf(TVDFClumpHeader));
  Log('Loaded "'+FName+'" ('+IntToStr(Count+1)+' files).');
  FreeAndNil(FStream);
  FLoaders := TVDFLoaders.Create;
  FLoaders.Resize( 128 );
end;

function TVDataFile.GetXMLDocument ( const FileName : Ansistring; DirName : Ansistring ) : TXMLDocument;
var iStream : TStream;
begin
  try
    iStream := GetFile( FileName, DirName );
    ReadXMLFile( Result, iStream, 'stream:' );
  finally
    FreeAndNil( iStream );
  end;
end;

function TVDataFile.GetFile(FileName: Ansistring; DirName: Ansistring): TStream;
var ID : DWord;
begin
  ID := GetFileID(FileName,DirName);
  
  if (vdfCompressed in FData[ID].Flags) and (vdfEncrypted in FData[ID].Flags) then
  begin
    GetFile := TFileStream.Create(FName,fmOpenRead);
    GetFile.Seek(FData[ID].Pos,soFromBeginning);
    GetFile := TIDEADeCryptStream.Create(DKKey,GetFile);
    TIDEADeCryptStream(GetFile).SourceOwner := True;
    GetFile := TDecompressionStream.Create(GetFile);
    TDecompressionStream(GetFile).SourceOwner := True;
  end
  else
  if vdfCompressed in FData[ID].Flags then
  begin
    GetFile := TFileStream.Create(FName,fmOpenRead);
    GetFile.Seek(FData[ID].Pos,soFromBeginning);
    GetFile := TDecompressionStream.Create(GetFile);
    TDecompressionStream(GetFile).SourceOwner := True;
  end
  else
  if vdfEncrypted in FData[ID].Flags then
  begin
    GetFile := TFileStream.Create(FName,fmOpenRead);
    GetFile.Seek(FData[ID].Pos,soFromBeginning);
    GetFile := TIDEADeCryptStream.Create(DKKey,GetFile);
    TIDEADeCryptStream(GetFile).SourceOwner := True;
  end
  else
  begin
    GetFile := TFileStream.Create(FName,fmOpenRead);
    GetFile.Seek(FData[ID].Pos,soFromBeginning);
  end;
end;

function TVDataFile.GetFileSize(FileName: Ansistring; DirName: Ansistring): Int64;
var ID : DWord;
begin
  ID := GetFileID(FileName,DirName);
  Exit(FData[ID].Size);
end;

function TVDataFile.FileExists(FileName: Ansistring; DirName: Ansistring): Boolean;
var Count : DWord;
begin
  for Count := 0 to FFiles-1 do
    if (FData[Count].Dir = DirName) and (FileName = FData[Count].Name) then Exit(True);
  Exit(False);
end;

procedure TVDataFile.Load(PackageID : AnsiString);
var Count    : DWord;
begin
  Log('VDF extraction ('+PackageID+')');

  for Count := 0 to FFiles-1 do
  begin
    if (FData[Count].Dir <> PackageID) then Continue;
    if (FData[Count].FType = 0)        then Continue;
    LoadFile( Count );
  end;
  Log('VDF extraction completed ('+PackageID+')');
end;

procedure TVDataFile.LoadFile(FileName: Ansistring; PackageID: AnsiString);
var ID    : DWord;
begin
  Log('VDF extraction ('+PackageID+'/'+FileName+')');
  ID := GetFileID(FileName,PackageID);
  LoadFile(ID);
  Log('VDF extraction complete ('+PackageID+'/'+FileName+')');
end;

procedure TVDataFile.RegisterLoader(LoaderID: DWord; Loader: TVDFLoader);
begin
  FLoaders[LoaderID] := Loader;
end;


destructor TVDataFile.Destroy;
begin
  FreeAndNil(FLoaders);
  inherited Destroy;
end;

procedure TVDataFile.LoadFile(FileID: DWord);
var DStream  : TStream;
    LSize    : DWord;
    Loader   : TVDFLoader;
begin
  if (FLoaders[FData[FileID].FType] = nil) then
    raise Exception.Create('Unregistered loader : "'+IntToStr(FData[FileID].FType)+'"!"');
  Loader   := FLoaders[FData[FileID].FType];
  DStream := GetFile(FData[FileID].Name,FData[FileID].Dir);
  LSize   := FData[FileID].Size;
  Loader(DStream,FData[FileID].Name,LSize);
  FreeAndNil(DStream);
end;

function TVDataFile.GetFileID(FileName, Dir: Ansistring): DWord;
var Count : DWord;
begin
  for Count := 0 to FFiles-1 do
    if (Dir = FData[Count].Dir) and (FileName = FData[Count].Name) then Exit(Count);
  raise EFOpenError.Create('File "'+Dir+'/'+FileName+'" not found in VDF "'+FName+'"!');
end;

end.

