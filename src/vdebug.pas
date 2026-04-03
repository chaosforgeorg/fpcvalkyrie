{$INCLUDE valkyrie.inc}
// @abstract(Debugging utilities for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
//
// Provides error logging, debug-to-string conversions, unhandled exception
// handler, and a diagnostic stream wrapper (TDebugStream).
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

unit vdebug;
interface
uses classes, sysutils, vutil;


const
// Error log file name
    ErrorLogFileName : AnsiString = 'error.log';

// Logs a string into the logfile.
procedure Log( const aLogString : Ansistring );
// Logs a string into the logfile. Parameters handled via pascal Format.
procedure Log( const aLogString : Ansistring; const aParam : array of Const );


// Logs a string into the logfile.
procedure Log( aLevel : TLogLevel; const aLogString       : Ansistring );
// Logs a string into the logfile. Parameters handled via Format.
procedure Log( aLevel : TLogLevel; const aLogString       : Ansistring; const aParam : array of Const );

// Error log open
procedure ErrorLogOpen( const aSeverity, aMessage : String );
// Error log write line (formatted)
procedure ErrorLogWriteln( const aText : String; const aFmt : array of const );
// Error log write line
procedure ErrorLogWriteln( const aText : String );
// Error log close
procedure ErrorLogClose;

// Converts a array of const VType to string debug representation.
// Exception safe.
function DebugToString( VT : PVarRec ) : AnsiString;

// Converts a variant to string.
// Exception safe.
function DebugToString( const V : Variant ) : AnsiString;

// Converts a array of const to string debug representation
// Exception safe.
function DebugToString( const arr : array of const ) : AnsiString;

type TDebugStream = class( TStream )
  private
    FStream:  TStream;
    function BytesToString(const Buffer; Count: Longint): Ansistring;
  public
    constructor Create( aStream: TStream );
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    destructor Destroy; override;
  end;

implementation
uses math, variants,vlog;

var ErrorStream : TFileStream;

procedure Log( const aLogString : Ansistring);
begin
  Log( LOGINFO, aLogString );
end;

procedure Log( const aLogString: Ansistring; const aParam: array of Const );
begin
  Log( LOGINFO, Format( aLogString, aParam) );
end;

procedure Log( aLevel : TLogLevel; const aLogString       : Ansistring);
begin
  Logger.Log( aLevel, aLogString );
end;

procedure Log( aLevel : TLogLevel; const aLogString: Ansistring; const aParam: array of const);
begin
  Log( aLevel, Format( aLogString,aParam ) );
end;

procedure ErrorLogOpen( const aSeverity, aMessage : String );
var iLine : AnsiString;
begin
  try
    if FileExists( ErrorLogFileName ) then
    begin
      ErrorStream := TFileStream.Create( ErrorLogFileName, fmOpenWrite );
      ErrorStream.Seek( 0, soFromEnd );
    end
    else
      ErrorStream := TFileStream.Create( ErrorLogFileName, fmCreate );
    iLine := '----------------------------------------------------------------------' + sLineBreak +
             'Timestamp   : ' + DateTimeToStr(Now) + sLineBreak +
             'Error level : ' + aSeverity + sLineBreak +
             'Message     : ' + aMessage + sLineBreak +
             sLineBreak;
    ErrorStream.Write( iLine[1], Length( iLine ) );
  except
    on e : Exception do
    begin
      FreeAndNil( ErrorStream );
      WriteLn( StdErr, 'WARNING: Can''t open error log: ' + e.Message );
    end;
  end;
end;

procedure ErrorLogWriteln( const aText : String; const aFmt : array of const );
begin
  ErrorLogWriteln( Format( aText, aFmt ) );
end;

procedure ErrorLogWriteln( const aText : String );
var iLine : AnsiString;
begin
  if ErrorStream = nil then Exit;
  try
    iLine := aText + sLineBreak;
    ErrorStream.Write( iLine[1], Length( iLine ) );
  except
  end;
end;

procedure ErrorLogClose;
var iLine : AnsiString;
begin
  if ErrorStream = nil then Exit;
  try
    iLine := '----------------------------------------------------------------------' + sLineBreak + sLineBreak;
    ErrorStream.Write( iLine[1], Length( iLine ) );
  except
  end;
  FreeAndNil( ErrorStream );
end;

function DebugToString(VT: PVarRec): AnsiString;
begin
try
  case VT^.vtype of
    vtinteger    : DebugToString := IntToStr( VT^.vinteger );
    vtint64      : DebugToString := IntToStr( VT^.vinteger );
    vtqword      : DebugToString := IntToStr( VT^.vinteger );
    vtboolean    : if VT^.vboolean then DebugToString := 'true' else DebugToString := 'false';
    vtchar       : DebugToString := VT^.vchar;
    vtextended   : DebugToString := FloatToStr( VT^.VExtended^ );
    vtString     : DebugToString := VT^.VString^;
    vtPointer    : DebugToString := '<pointer>';
    vtPChar      : DebugToString := VT^.VPChar;
    vtObject     : DebugToString := '<object>';
    vtClass      : DebugToString := '('+TClass(VT^.VClass).Classname+')';
    vtAnsiString : DebugToString := AnsiString(VT^.VAnsiString);
  else
      DebugToString := '<UNK:'+IntToStr(VT^.vtype)+'>';
  end;
except on e : Exception do
  DebugToString := 'exception on DebugToString'
end;
end;

function DebugToString ( const V : Variant ) : AnsiString;
begin
  case VarType(V) of
    varempty    : Exit('[empty]');
    varnull     : Exit('[null]');
    vardispatch : Exit('[dispatch]');
    varerror    : Exit('[error]');
    varvariant  : Exit('[variant]');
    varunknown  : Exit('[unknown]');
    varrecord   : Exit('[record]');
    varstrarg   : Exit('[strarg]');
    varany      : Exit('[any]');
    vartypemask : Exit('[typemask]');
    vararray    : Exit('[array]');
    varbyref    : Exit('[varbyref]');
  end;
  if VarType(V) > 256 then Exit('[array]');
  Exit( VarToStr( V ) );
end;

function DebugToString(const arr: array of const): AnsiString;
var i : Integer;
begin
try
  if High(arr) < 0 then
  begin
    Exit('()');
  end;
  DebugToString := '(';
  for i:=0 to High(arr) do
  begin
    if i <> 0 then DebugToString += ',';
    DebugToString += DebugToString(@(arr[i]));
  end;
  DebugToString += ')';
except on e : Exception do
  DebugToString := 'exception on DebugToString'
end;
end;

procedure UnhandledExceptionHandler(Obj : TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
var
  Message : String;
  i       : longint;
begin
  if Assigned( Logger ) then Logger.Flush;
  ErrorLogOpen( 'FATAL EXCEPTION', 'Fatal exception encountered' );
  {$HINTS OFF}
  ErrorLogWriteln('An unhandled exception occurred at $'+HexStr(PtrUInt(Addr),sizeof(PtrUInt)*2)+' :');
  {$HINTS ON}
  if Obj is exception then
   begin
     Message:=Exception(Obj).ClassName+' : '+Exception(Obj).Message;
     ErrorLogWriteln(Message);
   end
  else
   ErrorLogWriteln('Exception object '+Obj.ClassName+' is not of class Exception.');
  ErrorLogWriteln(BackTraceStrFunc(Addr));
  if (FrameCount>0) then
    begin
      for i:=0 to FrameCount-1 do
        ErrorLogWriteln(BackTraceStrFunc(Frames[i]));
    end;
  ErrorLogWriteln('');
  ErrorLogClose;
end;

function TDebugStream.BytesToString( const Buffer; Count: Longint ): Ansistring;
const
  MaxLogSize = 64;
var
  Tmp: Ansistring;
  i: Integer;
  Ch: Char;
begin
  Initialize( Tmp );
  SetLength(Tmp, Min(Count, MaxLogSize));
  Move(Buffer, PChar(Tmp)^, Length(Tmp));

  for i := 1 to Length(Tmp) do
  begin
    Ch := Tmp[i];
    if not (Ch in [#32..#126]) then
    begin
      Tmp[i] := '.'; // replace non-printable characters with a dot
    end;
  end;

  Result := Tmp;

  if Count > MaxLogSize then
  begin
    Result := Result + '...';
  end;
end;

constructor TDebugStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TDebugStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
  Log( LOGINFO, 'Read: %d bytes, Content: %s', [Result, BytesToString(Buffer, Result)]);
end;

function TDebugStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);
  Log( LOGINFO, 'Write: %d bytes, Content: %s', [Result, BytesToString(Buffer, Result)]);
end;

function TDebugStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin);
  Log( LOGINFO, 'Seek: Offset: %d, Origin: %d', [Offset, Ord(Origin)] );
end;

destructor TDebugStream.Destroy;
begin
  FreeAndNil( FStream );
  inherited Destroy;
end;

initialization

exceptproc := @UnhandledExceptionHandler;
ErrorStream := nil;

end.

{ $Log
  -- 14.I.2006 Added AnsiString support
  -- Created 2.06.2003
}
