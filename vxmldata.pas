{$INCLUDE valkyrie.inc}
// @abstract(XML utilities wrapper class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-01-14 22:16:41 +0100 (Mon, 14 Jan 2008) $)
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

unit vxmldata;
interface

uses Classes, SysUtils, DOM, XMLRead, XMLWrite, vnode, vxml;

type TScoreFile  = class;
type TScoreEntry = TDOMElement;

type

{ TScoreEntryEumerator }

TScoreEntryEumerator = object
protected
  FScoreFile : TScoreFile;
  FPosition  : Integer;
  function GetCurrent: TScoreEntry;
public
  constructor Create( aScoreFile : TScoreFile );
  function MoveNext: Boolean;
  property Current: TScoreEntry read GetCurrent;
end;

type

{ TVXMLDataFile }

TVXMLDataFile = class( TVObject )
  constructor Create( const aFilePath, aRootID : AnsiString );
  procedure SetCRC( const aKey1, aKey2 : AnsiString );
  procedure SetBackup( const aBackupPath : AnsiString; aMaxBackupCount : DWord );
  procedure Backup;
  function GetLastBackup : AnsiString;
  function Load : Boolean; virtual;
  procedure Save; virtual;
  destructor Destroy; override;
protected
  procedure CreateNew; virtual;
protected
  FFilePath   : AnsiString;
  FBackupPath : AnsiString;
  FRootID     : AnsiString;
  FKey1       : AnsiString;
  FKey2       : AnsiString;
  FMaxBackup  : DWord;
  FXML        : TVXMLDocument;
public
  property FilePath  : AnsiString    read FFilePath;
  property RootID    : AnsiString    read FRootID;
  property XML       : TVXMLDocument read FXML;
end;

{ TScoreFile }

TScoreFile = class( TVXMLDataFile )
  constructor Create( const aFilePath : AnsiString; aMaxEntries : DWord; const aRootID : AnsiString = 'scores' );
  function Load : Boolean; override;
  function Add( aScore : LongInt ) : TScoreEntry;
  function GetEnumerator: TScoreEntryEumerator;
  function GetEntry( aEntry : DWord ) : TScoreEntry;
protected
  FMaxEntries : DWord;
  FLastEntry  : DWord;
  FEntries    : DWord;
public
  property LastEntry : DWord                      read FLastEntry;
  property Entries   : DWord                      read FEntries;
  property Entry[ aIndex : DWord ] : TScoreEntry  read GetEntry; default;
end;

implementation

uses zstream, vutil;

{ TScoreEntryEumerator }

function TScoreEntryEumerator.GetCurrent: TScoreEntry;
begin
  Result := TScoreEntry( FScoreFile.FXML.DocumentElement.ChildNodes.Item[FPosition] );
end;

constructor TScoreEntryEumerator.Create( aScoreFile : TScoreFile );
begin
  FScoreFile := aScoreFile;
  FPosition  := -1;
end;

function TScoreEntryEumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FScoreFile.FEntries;
end;

{ TVXMLDataFile }

constructor TVXMLDataFile.Create(const aFilePath, aRootID : AnsiString);
begin
  FFilePath   := aFilePath;
  FRootID     := aRootID;
  FXML        := nil;
  FKey1       := '';
  FKey2       := '';
  FBackupPath := '';
  FMaxBackup  := 0;
end;

procedure TVXMLDataFile.SetCRC(const aKey1, aKey2: AnsiString);
begin
  FKey1 := aKey1;
  FKey2 := aKey2;
end;

procedure TVXMLDataFile.SetBackup(const aBackupPath: AnsiString;  aMaxBackupCount: DWord);
begin
  FBackupPath := aBackupPath;
  FMaxBackup  := aMaxBackupCount;
end;

procedure TVXMLDataFile.Backup;
var iInfo    : TSearchRec;
    iStamp   : String;
    iFound   : String;
    iRawName : AnsiString;
    iMin     : LongInt;
    iCount   : DWord;
begin
  if FMaxBackup = 0 then Exit;
  iMin     := 99999999;
  iStamp   := FormatDateTime('YYYYMMDD',Now);
  iCount   := 0;
  iRawName := ExtractFileName( FFilePath );
  if FindFirst (FBackupPath+iRawName+'.backup-*',faAnyFile and faDirectory,iInfo)=0 then
  begin
    repeat
      Inc(iCount);
      iFound := RightStr( iInfo.Name, 8 );
      if iFound = iStamp then
      begin
        DeleteFile(FBackupPath+iInfo.Name);
        Break;
      end;
      if StrToInt( iFound ) < iMin then iMin := StrToInt( iFound );
    until FindNext(iInfo)<>0;
    if iCount > FMaxBackup then
      DeleteFile(FBackupPath+iRawName+IntToStr( iMin ) );
  end;
  FileCopy( FFilePath, FBackupPath+iRawName+'.backup-'+iStamp );
  FindClose( iInfo );
end;

function TVXMLDataFile.GetLastBackup : AnsiString;
var iInfo    : TSearchRec;
    iRawName : AnsiString;
    iFound   : String;
    iMax     : LongInt;
begin
  if FMaxBackup = 0 then Exit('');
  iRawName := ExtractFileName( FFilePath );
  iMax   := 0;
  if FindFirst (FBackupPath+iRawName+'.backup-*',faAnyFile and faDirectory,iInfo)=0 then
  repeat
    iFound := RightStr( iInfo.Name, 8 );
    if StrToInt( iFound ) > iMax then iMax := StrToInt( iFound );
  until FindNext( iInfo ) <> 0;
  FindClose( iInfo );
  if iMax = 0 then Exit('');
  Exit( FBackupPath+iRawName+'.backup-'+IntToStr( iMax ) );
end;

procedure TVXMLDataFile.Save;
var iStream : TGZFileStream;
begin
  if FXML = nil then Exit;
  if FKey1 <> '' then
    FXML.DocumentElement.SetAttribute('crc', XML.CRC( FKey1, FKey2 ) );
  iStream := TGZFileStream.Create( FFilePath, gzOpenWrite );
  WriteXMLFile( XML, iStream );
  FreeAndNil( iStream );
end;

procedure TVXMLDataFile.CreateNew;
var iNode : TDOMNode;
begin
  FreeAndNil( FXML );
  FXML  := TVXMLDocument.Create;
  iNode := FXML.CreateElement(FRootID);
  FXML.AppendChild( iNode );
end;

function TVXMLDataFile.Load : Boolean;
var iCRC, iReadCRC : AnsiString;
    iFilePath      : AnsiString;
    iStream        : TGZFileStream;
    iDocument      : TXMLDocument;
    iSecondTry     : Boolean;
begin
  FreeAndNil( FXML );
  if not FileExists( FFilePath ) then
  begin
    CreateNew;
    Exit( False );
  end;
  iFilePath := FFilePath;
  Load := False;
  iSecondTry := False;
  FXML := nil;
  repeat
    try
      iStream := TGZFileStream.Create( iFilePath, gzOpenRead );
      try
        ReadXMLFile( iDocument, iStream );
      finally
        FreeAndNil( iStream );
      end;
      if FKey1 <> '' then
      begin
        iCRC      := TVXMLDocument( iDocument ).CRC(FKey1,FKey2);
        iReadCRC  := iDocument.DocumentElement.GetAttribute('crc');
        if iCRC <> iReadCRC then
        begin
          FreeAndNil( iDocument );
          raise Exception.Create('Corrupted '+iFilePath+' file!');
        end;
      end;
      FXML := TVXMLDocument( iDocument );
      Load := True;
    except
      on e : Exception do
      begin
        if FileExists( iFilePath + '-broken' ) then DeleteFile( iFilePath + '-broken' );
        FileCopy( iFilePath, iFilePath + '-broken' );
        DeleteFile( iFilePath );
        Log( LOGERROR, 'Error while loading '+iFilePath+' : '+ e.message );
        CreateNew;
        if iSecondTry then
        begin
          Log( LOGERROR, 'Backup also corrupted. Creating new data file...' );
          Break;
        end;
        iSecondTry := True;
        iFilePath := GetLastBackup;
        if iFilePath = '' then
        begin
          Log( LOGERROR, 'No backups present. Creating new data file...');
          Break;
        end;
      end;
    end;
  until Load;

  if Load then
  begin
    if not iSecondTry then Backup;
  end
  else
    CreateNew;
end;

destructor TVXMLDataFile.Destroy;
begin
  Save;
  inherited Destroy;
end;

{ TScoreFile }

constructor TScoreFile.Create(const aFilePath: AnsiString; aMaxEntries : DWord; const aRootID: AnsiString);
begin
  inherited Create( aFilePath, aRootID );
  FMaxEntries := aMaxEntries;
  FLastEntry  := 0;
  FEntries    := 0;
end;

function TScoreFile.Load : Boolean;
begin
  Load := inherited Load;
  FEntries := FXML.DocumentElement.ChildNodes.Count;
end;

function TScoreFile.Add( aScore: LongInt ): TScoreEntry;
var iInserted   : Boolean;
    iCount      : DWord;
    iXMLValue   : TDOMNode;
begin
  if FEntries >= FMaxEntries then
  begin
    if aScore <= StrToInt(TDOMElement(FXML.DocumentElement.LastChild).GetAttribute('score') ) then Exit( nil );
  end;

  Result := FXML.CreateElement('entry');
  Result.SetAttribute('score', IntToStr(aScore) );
  iInserted := False;
  if FEntries > 0 then
  for iCount := 0 to FEntries-1 do
  begin
    iXMLValue := FXML.DocumentElement.ChildNodes.Item[iCount];
    if StrToInt(TDOMElement(iXMLValue).GetAttribute('score')) < aScore then
    begin
      FXML.DocumentElement.InsertBefore(Result,iXMLValue);
      iInserted  := True;
      FLastEntry := iCount+1;
      break;
    end;
  end;
  if not iInserted then
  begin
    FXML.DocumentElement.AppendChild(Result);
    FLastEntry := FEntries+1;
  end;
  Inc( FEntries );

  while FEntries > FMaxEntries do
  begin
    iXMLValue := FXML.DocumentElement.LastChild;
    if iXMLValue = Result then Result := nil;
    FXML.DocumentElement.RemoveChild( FXML.DocumentElement.LastChild );
    Dec( FEntries );
  end;
end;

function TScoreFile.GetEnumerator: TScoreEntryEumerator;
begin
  GetEnumerator.Create( Self );
end;

function TScoreFile.GetEntry(aEntry: DWord): TScoreEntry;
begin
  if (aEntry = 0) or (aEntry > FEntries) then Exit( nil );
  Exit( TScoreEntry( FXML.DocumentElement.ChildNodes.Item[aEntry-1] ));
end;


end.

