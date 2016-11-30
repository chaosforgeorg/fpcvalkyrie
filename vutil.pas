{$INCLUDE valkyrie.inc}
// @abstract(Valkyrie Core - Valkyrie Utilities)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(7 May, 2004)
// @lastmod(23 Nov, 2005)
//
// Common utility functions, defines and objects for Valkyrie.
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

unit vutil;
interface
uses SysUtils;

type
// Type for 32bit flags
     T32BitRange  = 0..31;
// 256 bit flags type
     TFlags       = set of Byte;
// Pointer to TFlags
     PFlags       = ^TFlags;
// 32 bit flags type
     TFlags32     = set of T32BitRange;
// Unique IDentification number. QWord may prove not enough?
     TUID         = QWord;
// Message IDentification number.
     TMSGID       = Word;
// Type identification number. Used to save/load.
     TIDN         = AnsiString;
// Translation table of Printable characters
     TPrintableCharToByte = array[33..126] of Byte;
//
     TAnsiStringArray = array of AnsiString;
//
     TVersion     = array [1..4] of Byte;
     // Log levels. LOGNONE means no logging.
     // => level of verbosity increased from left to right
     TLogLevel    = (LOGNONE, LOGREPORT, LOGERROR, LOGWARN, LOGINFO, LOGDEBUG, LOGDEBUG2);

const
  Black        = 0;  //< Color black
  Blue         = 1;  //< Color blue
  Green        = 2;  //< Color green
  Cyan         = 3;  //< Color cyan
  Red          = 4;  //< Color red
  Magenta      = 5;  //< Color magenta
  Brown        = 6;  //< Color brown
  LightGray    = 7;  //< Color light gray
  DarkGray     = 8;  //< Color dark gray
  LightBlue    = 9;  //< Color light blue
  LightGreen   = 10; //< Color light green
  LightCyan    = 11; //< Color light cyan
  LightRed     = 12; //< Color light red
  LightMagenta = 13; //< Color light magenta
  Yellow       = 14; //< Color yellow
  White        = 15; //< Color white
  
// Reads a single line from the given textfile and returns it.
// Used by many Valkyrie projects for versioning.
//
// @param( aFileName Text file with first line as version string )
// @returns( First line of the text file )
// @raises( EException if file not found or cannot be read )
function ReadVersion( const aFileName : string ) : string; deprecated;

// A virtual die. number is the number of dice, sides the number of sides.
// Note that Randomize must be called before. If any of the parameters is
// zero, the result is zero as well.
//
// @param( aNumber number of dice to be rolled )
// @param( aSides number of sides on the dice )
// @returns( Result of the dice roll )
function Dice( aNumber : DWord; aSides : DWord ) : DWord;

// Converts string to a boolean.
//
// @param( aString String to be converted )
// @returns( @true if string starts with 't', 'T', 'y', 'Y', @false otherwise )
function StrToBool( const aString : AnsiString ) : Boolean;

// Converts boolean to a string.
//
// @param( aBoolean Boolean to be converted )
// @returns( 'TRUE' if aBoolean is @true, 'FALSE' if @false )
function BoolToStr(const aBoolean : boolean) : string;

// Reads a parameter from the string. Depending on the length of aParamChar:
// Lenght 1 returns delimeted word number aNumber, length 2 returns the word
// between the two characters, length three - parameter delimeted by character
// number 2, between characters 1 and 3.
//
// @deprecated
// Use ExtractDelimited and ExtractSubStr instead.
function Parameter( const aString : AnsiString; aNumber : Byte; const aParamChar: AnsiString ) : AnsiString; deprecated;

// Crops the String to given length. Returned string will be at most
// the passed length.
//
// @param( aString String to be cropped )
// @param( aLength Length the string is to be cropped to )
// @returns( String cropped to the desired length )
function CroppedString( const aString : Ansistring; aLength : Word ) : Ansistring;

// Sets a string to given length in place, cropping it if needed, and padding
// it with spaces or the passed character if length is greater than the one
// passed.
//
// @param( aString String to be padded to the desired length )
// @param( aLength Length the string is to be padded to )
// @param( aPadChar Padding character, space by default )
procedure Pad( var aString : AnsiString; aLength : byte; aPadChar : Char = ' ' );

// Produces a string to given length from the one passed, cropping it if needed,
// and padding it with spaces or the passed character if length is greater than
// the one passed.
//
// @param( aString Source string to be padded to the desired length )
// @param( aLength Length the string is to be padded to )
// @param( aPadChar Padding character, space by default )
function Padded( const aString : AnsiString; aLength : byte; aPadChar : Char = ' ' ) : AnsiString;

// Returns the string with the first letter capitalized.
// Deprecated, use Capitalized instead.
function CapLet(const s : Ansistring):Ansistring; deprecated;

// Produces a time string of the format DD.MM.YY HH:MM.
//
// @returns( DateTimeToStr( Now ) )
function TimeStamp : string;

// Splits a string into two substrings. Split is performed at the first split
// character position after split distance. The split character is not present
// in any of the output strings.
//
// If the split character is not present, the string is returned in the first
// string, if it is the first character, then the string is returned in the
// second one.
//
// @param( aString String to be splitted )
// @param( aFirstResult String to the left of the split character )
// @param( aSecondResult String to the right of the split character )
// @param( aSplitChar The character to split the string, space by default )
// @param( aSplitPos The position from which to search for the split character,
//         zero by default )
procedure Split( const aString : Ansistring;
                 out aFirstResult, aSecondResult : Ansistring;
                 aSplitChar : Char = ' '; aSplitDist : Byte = 0); deprecated;

// Formats the string the Valkyrie way -- replacing @1..@X with the passed
// parameters. Supports conversion for integers and floats.
//
// @param( aString String to be formated )
// @param( aParams Parameters to be converted and injected into the string )
function VFormat( const aString : Ansistring; const aParams : array of Const ) : Ansistring; deprecated;

// Returns the string with the first letter capitalized.
//
// @param( aString String to be capitalized )
// @returns( Capitalized string )
function Capitalized( const aString : Ansistring ) : Ansistring;

// Sequential non-existing filename return
function SequentialFilename( const Name, Ext : Ansistring; Numbers : Byte = 4) : Ansistring;

// Converts version string of the format "x.y.z trail..." to TVersion format
function StringToVersion( const VersionString : AnsiString ) : TVersion;

// Converts version string of the format "x.y.z trail..." to TVersion format
function ArrayToVersion( const VersionArray : array of Byte ) : TVersion;

// Converts TVersion to string
function VersionToString( const Version : TVersion ) : Ansistring;

type TPoint = object
  X, Y : Integer;
  procedure Init( aX, aY : Integer );
  function toString : AnsiString;
end;
type PPoint = ^TPoint;

type TRectangle = object
  Pos : TPoint;
  Dim : TPoint;
  procedure Init( aPos, aDim : TPoint );
  procedure Init( aPos : TPoint; aWidth, aHeight : Integer );
  procedure Init( aX, aY, aWidth, aHeight : Integer );
  function toString : AnsiString;
  function GetX2 : Integer;
  function GetY2 : Integer;
  function GetPos2 : TPoint;
  function GetCenter : TPoint;

  function Shrinked( i : Integer = 1 ) : TRectangle;
  procedure Shrink( i : Integer = 1 );
  function Expanded( i : Integer = 1 ) : TRectangle;
  procedure Expand( i : Integer = 1 );
  function Shrinked( x,y  : Integer ) : TRectangle;
  procedure Shrink( x,y : Integer );
  function Expanded( x,y : Integer ) : TRectangle;
  procedure Expand( x,y : Integer );

  property TopLeft : TPoint read Pos;
  function BottomRight : TPoint;
  function TopRight : TPoint;
  function BottomLeft : TPoint;

  property x : Integer read Pos.x write Pos.x;
  property y : Integer read Pos.y write Pos.y;
  property w : Integer read Dim.x write Dim.x;
  property h : Integer read Dim.y write Dim.y;
  property x2 : Integer read GetX2;
  property y2 : Integer read GetY2;
  property pos2 : TPoint read GetPos2;
end;
type PRectangle = ^TRectangle;

function Point( aX, aY : Integer ) : TPoint;
function Rectangle( aPos, aDim : TPoint ) : TRectangle;
function Rectangle( aPos : TPoint; aWidth, aHeight : Integer ) : TRectangle;
function Rectangle( aX, aY, aWidth, aHeight : Integer ) : TRectangle;

operator in( a : TPoint; b : TRectangle) r : Boolean; inline;
operator = (a,b : TPoint) r : boolean; inline;
operator + (a,b : TPoint) r : TPoint; inline;
operator - (a,b : TPoint) r : TPoint; inline;
operator * (a : TPoint; b : Integer) r : TPoint; inline;
operator + (a : TRectangle; b : TPoint) r : TRectangle; inline;
operator - (a : TRectangle; b : TPoint) r : TRectangle; inline;

type TRectangleEnumerator = object
private
  FCurrent : TPoint;
  FA, FB   : TPoint;
public
  constructor Create( const Rect : TRectangle );
  function MoveNext : Boolean;
  property Current : TPoint read FCurrent;
end;

operator enumerator( a : TRectangle ) : TRectangleEnumerator;

const PointZero : TPoint = ( x : 0; y : 0; );
const PointUnit : TPoint = ( x : 1; y : 1; );

function Max( a, b : TPoint ) : TPoint; overload;
function Min( a, b : TPoint ) : TPoint; overload;

// Valkyrie's base exception class. Allows construction via VFormat.
type EException = class(Exception)
  // Create the exception, just a override of Exception.Create
  constructor Create( const aMessage : AnsiString );
  // Create the exception, formating the arguments.
  constructor Create( const aMessage : AnsiString; const aParams : array of Const );
end;


operator = ( a : TVersion; b : TVersion) : Boolean;
operator > ( a : TVersion; b : TVersion) : Boolean;
operator >= ( a : TVersion; b : TVersion) : Boolean;

type TVarRecArr = array of TVarRec;

function ConcatConstArray( const A,B : array of const ) : TVarRecArr;
function RPosSetEx( const c : TSysCharSet; const s : ansistring; count : Integer ) : Integer;
function IIf( aCondition : Boolean; const aTrue : AnsiString; const aFalse : AnsiString = '' ) : AnsiString;
function IIf( aCondition : Boolean; aTrue : LongInt; aFalse : LongInt = 0 ) : LongInt;
function FileCopy( const aSourcePath, aDestPath : AnsiString ) : Boolean;

implementation
uses math,strutils;

function ConcatConstArray( const A,B : array of const ) : TVarRecArr;
var AH,BH,C : Integer;
begin
  AH := High( A );
  BH := High( B );
  SetLength( ConcatConstArray, AH+BH+2 );
  if AH >= 0 then
  for C := 0 to AH do
    ConcatConstArray[ C ] := A[ C ];
  if BH >= 0 then
  for C := 0 to BH do
    ConcatConstArray[ C+AH+1 ] := B[ C ];
end;

function RPosSetEx( const c : TSysCharSet; const s : AnsiString; Count : Integer ) : Integer;
var j : Integer;
begin
  if pchar( pointer(s) ) = nil then Exit( 0 );
  j := count;
  if (j > length(s)) or (j = 0) then Exit(0);
  while ( j > 0 ) and ( not ( s[j] in c ) ) do Dec(j);
  Exit( j );
end;

function IIf(aCondition: Boolean; const aTrue: AnsiString; const aFalse: AnsiString): AnsiString;
begin
  if aCondition then Exit( aTrue ) else Exit( aFalse );
end;

function IIf(aCondition: Boolean; aTrue: LongInt; aFalse: LongInt): LongInt;
begin
  if aCondition then Exit( aTrue ) else Exit( aFalse );
end;

function FileCopy(const aSourcePath, aDestPath: AnsiString): Boolean;
var iHSrc   : Integer;
    iHDst   : Integer;
    iLength : Integer;
    iSize   : Longint;
    iBuffer : packed array [0..2047] of Byte;
begin
  Result := False;
  if aSourcePath = aDestPath then Exit( False );

  iHSrc := FileOpen( aSourcePath, fmOpenRead );
  if iHSrc >= 0 then
  begin
    iSize := FileSeek( iHSrc, 0, 2 );
    FileSeek( iHSrc, 0, 0 );
    iHDst := FileCreate( aDestPath );
    if iHDst >= 0 then
    begin
      while iSize > 0 do
      begin
        iLength := FileRead( iHSrc, iBuffer, SizeOf(iBuffer) );
        FileWrite(iHDst,iBuffer,iLength);
        iSize := iSize - iLength;
      end;
      FileSetDate( iHDst, FileGetDate( iHSrc ) );
      FileClose( iHDst );
      FileSetAttr( aDestPath, FileGetAttr(aSourcePath) );
      Result := True;
    end;
    FileClose( iHSrc );
  end;
end;

function Point ( aX, aY : Integer ) : TPoint;
begin
  Point.x := aX;
  Point.y := aY;
end;

function Rectangle ( aPos, aDim : TPoint ) : TRectangle;
begin
  Rectangle.Pos := aPos;
  Rectangle.Dim := aDim;
end;

function Rectangle ( aPos : TPoint; aWidth, aHeight : Integer ) : TRectangle;
begin
  Rectangle.Pos   := aPos;
  Rectangle.Dim.x := aWidth;
  Rectangle.Dim.y := aHeight;
end;

function Rectangle ( aX, aY, aWidth, aHeight : Integer ) : TRectangle;
begin
  Rectangle.Pos.x := aX;
  Rectangle.Pos.y := aY;
  Rectangle.Dim.x := aWidth;
  Rectangle.Dim.y := aHeight;
end;

operator in ( a : TPoint; b : TRectangle ) r : Boolean;
begin
  r := (a.x >= b.Pos.x) and (a.y >= b.Pos.y)
    and (a.x < b.pos.x + b.dim.x)
    and (a.y < b.pos.y + b.dim.y);
end;

operator = ( a, b : TPoint ) r : boolean;
begin
  r := (a.x = b.x) and (a.y = b.y) ;
end;

operator + ( a, b : TPoint ) r : TPoint;
begin
  r.x := a.x + b.x;
  r.y := a.y + b.y;
end;

operator - ( a, b : TPoint ) r : TPoint;
begin
  r.x := a.x - b.x;
  r.y := a.y - b.y;
end;

operator * ( a : TPoint; b : Integer ) r : TPoint;
begin
  r.x := a.x * b;
  r.y := a.y * b;
end;

operator + ( a : TRectangle; b : TPoint ) r : TRectangle;
begin
  r.pos := a.pos + b;
  r.dim := a.dim;
end;

operator - ( a : TRectangle; b : TPoint ) r : TRectangle;
begin
  r.pos := a.pos - b;
  r.dim := a.dim;
end;

operator enumerator ( a : TRectangle ) : TRectangleEnumerator;
begin
  Result.Create(a);
end;

function Max ( a, b : TPoint ) : TPoint;
begin
  Result.x := math.Max( a.x, b.x );
  Result.y := math.Max( a.y, b.y );
end;

function Min ( a, b : TPoint ) : TPoint;
begin
  Result.x := math.Min( a.x, b.x );
  Result.y := math.Min( a.y, b.y );
end;

operator = ( a : TVersion; b : TVersion) : Boolean;
begin
  Exit( (a[1] = b[1]) and (a[2] = b[2]) and (a[3] = b[3]) and (a[4] = b[4]) );
end;

operator >= ( a : TVersion; b : TVersion) : Boolean;
begin
  if a[1] <> b[1] then Exit( a[1] > b[1] );
  if a[2] <> b[2] then Exit( a[2] > b[2] );
  if a[3] <> b[3] then Exit( a[3] > b[3] );
  if a[4] <> b[4] then Exit( a[4] > b[4] );
  Exit( True );
end;

operator > ( a : TVersion; b : TVersion) : Boolean;
begin
  if a[1] <> b[1] then Exit( a[1] > b[1] );
  if a[2] <> b[2] then Exit( a[2] > b[2] );
  if a[3] <> b[3] then Exit( a[3] > b[3] );
  if a[4] <> b[4] then Exit( a[4] > b[4] );
  Exit( False );
end;

{ TRectangleEnumerator }

constructor TRectangleEnumerator.Create ( const Rect : TRectangle ) ;
begin
  FA := Rect.Pos;
  FB := Rect.Pos + Rect.Dim - PointUnit;
  FCurrent := FA;
  FCurrent.x -= 1;
end;

function TRectangleEnumerator.MoveNext : Boolean;
begin
  FCurrent.x += 1;
  if FCurrent.x > FB.x then
  begin
    FCurrent.y += 1;
    FCurrent.x := FA.x;
  end;
  Exit( FCurrent.y <= FB.y );
end;

{ TRectangle }

procedure TRectangle.Init ( aPos, aDim : TPoint ) ;
begin
  Pos := aPos;
  Dim := aDim;
end;

procedure TRectangle.Init ( aPos : TPoint; aWidth, aHeight : Integer ) ;
begin
  Pos   := aPos;
  Dim.x := aWidth;
  Dim.y := aHeight;
end;

procedure TRectangle.Init ( aX, aY, aWidth, aHeight : Integer ) ;
begin
  Pos.x := aX;
  Pos.y := aY;
  Dim.x := aWidth;
  Dim.y := aHeight;
end;

function TRectangle.toString : AnsiString;
begin
  Exit( Format('%sx%s',[Pos.toString,Dim.toString]) );

end;

function TRectangle.GetX2 : Integer;
begin
  Exit( Pos.x + Dim.x - 1 );
end;

function TRectangle.GetY2 : Integer;
begin
  Exit( Pos.y + Dim.y - 1 );
end;

function TRectangle.GetPos2 : TPoint;
begin
  GetPos2.x := Pos.x + Dim.x - 1;
  GetPos2.y := Pos.y + Dim.y - 1;
end;

function TRectangle.GetCenter : TPoint;
begin
  GetCenter.x := Pos.x + Dim.x div 2;
  GetCenter.y := Pos.y + Dim.y div 2;
end;

function TRectangle.Shrinked ( i : Integer ) : TRectangle;
begin
  Shrinked.Pos := Pos + PointUnit*i;
  Shrinked.Dim := Dim - PointUnit*2*i;
end;

procedure TRectangle.Shrink ( i : Integer ) ;
begin
  Pos := Pos + PointUnit*i;
  Dim := Dim - PointUnit*2*i;
end;

function TRectangle.Expanded ( i : Integer ) : TRectangle;
begin
  Expanded.Pos := Pos - PointUnit*i;
  Expanded.Dim := Dim + PointUnit*2*i;
end;

procedure TRectangle.Expand ( i : Integer ) ;
begin
  Pos := Pos - PointUnit*i;
  Dim := Dim + PointUnit*2*i;
end;

function TRectangle.Shrinked ( x, y : Integer ) : TRectangle;
begin
  Shrinked.Pos := Pos + Point(x,y);
  Shrinked.Dim := Dim - Point(x,y)*2;
end;

procedure TRectangle.Shrink ( x, y : Integer ) ;
begin
  Pos := Pos + Point(x,y);
  Dim := Dim - Point(x,y)*2;
end;

function TRectangle.Expanded ( x, y : Integer ) : TRectangle;
begin
  Expanded.Pos := Pos - Point(x,y);
  Expanded.Dim := Dim + Point(x,y)*2;
end;

procedure TRectangle.Expand ( x, y : Integer ) ;
begin
  Pos := Pos - Point(x,y);
  Dim := Dim + Point(x,y)*2;
end;

function TRectangle.BottomRight : TPoint;
begin
  Exit( Pos + Dim - PointUnit );
end;

function TRectangle.TopRight : TPoint;
begin
  Exit( Pos + Point( Dim.x - 1, 0 ) );
end;

function TRectangle.BottomLeft : TPoint;
begin
  Exit( Pos + Point( 0, Dim.y - 1 ) );
end;

{ TPoint }

procedure TPoint.Init ( aX, aY : Integer ) ;
begin
  x := aX;
  y := aY;
end;

function TPoint.toString : AnsiString;
begin
  Exit( Format('(%d,%d)',[x,y]) );
end;

function VFormat( const aString : Ansistring; const aParams : array of Const) : Ansistring;
var iCount : DWord;
begin
  VFormat := aString;
  for iCount := 1 to High( aParams ) + 1 do
  begin
    case aParams[ iCount - 1 ].vtype of
      vtInt64      : VFormat := AnsiReplaceStr( VFormat, '@' + IntToStr( iCount ), IntToStr( aParams[iCount-1].VInt64^ ) );
      vtQWord      : VFormat := AnsiReplaceStr( VFormat, '@' + IntToStr( iCount ), IntToStr( aParams[iCount-1].VQWord^ ) );
      vtInteger    : VFormat := AnsiReplaceStr( VFormat, '@' + IntToStr( iCount ), IntToStr( aParams[iCount-1].vinteger ) );
      vtString     : VFormat := AnsiReplaceStr( VFormat, '@' + IntToStr( iCount ), aParams[iCount-1].vstring^ );
      vtExtended   : VFormat := AnsiReplaceStr( VFormat, '@' + IntToStr( iCount ), FloatToStr( aParams[iCount-1].vextended^ ) );
      vtAnsiString : VFormat := AnsiReplaceStr( VFormat, '@' + IntToStr( iCount ), AnsiString( aParams[iCount-1].vansistring ) );
    end;
  end;
end;

function Capitalized( const aString: Ansistring ): Ansistring;
begin
  Capitalized    := aString;
  Capitalized[1] := UpCase( Capitalized[1] );
end;

function SequentialFilename(const Name, Ext: Ansistring; Numbers : Byte = 4 ): Ansistring;
var Count : DWord;
    Full  : AnsiString;
begin
  Count := 0;
  repeat
    Full := IntToStr(Count);
    if Length(Full) > Numbers then Exit('');
    Full := Name + AddChar( '0', IntToStr(Count), Numbers ) + Ext;
    Inc(Count);
  until not FileExists(Full);
  Exit(Full);
end;

function StringToVersion( const VersionString : AnsiString ) : TVersion;
var Temp  : AnsiString;
    CB,CE : Byte;
begin
  for CB := 1 to 4 do StringToVersion[CB] := 0;
  if Length(VersionString) = 0 then Exit;
  CB := 1;
  while (CB <= Length(VersionString)) and (not (VersionString[CB] in ['0','1'..'9'])) do Inc(CB);
  CE := CB;
  while (CE <= Length(VersionString)) and (VersionString[CE] in ['0','1'..'9','.']) do Inc(CE);
  Temp := MidStr(VersionString, CB, CE-CB);
  StringToVersion[1] := StrToIntDef( ExtractDelimited( 1, Temp, ['.'] ), 0 );
  StringToVersion[2] := StrToIntDef( ExtractDelimited( 2, Temp, ['.'] ), 0 );
  StringToVersion[3] := StrToIntDef( ExtractDelimited( 3, Temp, ['.'] ), 0 );
  StringToVersion[4] := StrToIntDef( ExtractDelimited( 4, Temp, ['.'] ), 0 );
end;

function ArrayToVersion(const VersionArray: array of Byte): TVersion;
begin
  ArrayToVersion[1] := VersionArray[Low(VersionArray)  ];
  ArrayToVersion[2] := VersionArray[Low(VersionArray)+1];
  ArrayToVersion[3] := VersionArray[Low(VersionArray)+2];
  if Length( VersionArray ) > 3
    then ArrayToVersion[4] := VersionArray[Low(VersionArray)+3]
    else ArrayToVersion[4] := 0;
end;

function VersionToString( const Version : TVersion ) : Ansistring;
begin
  VersionToString := IntToStr( Version[1] )+'.'+IntToStr( Version[2] )+'.'+IntToStr( Version[3] );
  if Version[4] <> 0 then VersionToString += '.'+IntToStr( Version[4] );
end;

procedure Split( const aString : Ansistring;
                 out aFirstResult, aSecondResult : Ansistring;
                 aSplitChar : Char = ' '; aSplitDist : Byte = 0 );
var iSplitCharPos : Word;
begin
  if aSplitDist = 0 then
    iSplitCharPos := Pos( aSplitChar, aString )
  else
  begin
    iSplitCharPos := aSplitDist;
    if iSplitCharPos < Length( aString )
      then while ( iSplitCharPos > 0 ) and ( aString[iSplitCharPos] <> aSplitChar ) do Dec( iSplitCharPos )
      else iSplitCharPos := 0;
  end;

  if iSplitCharPos = 0 then begin aFirstResult := aString; aSecondResult := '';      Exit; end;
  if iSplitCharPos = 1 then begin aFirstResult := '';      aSecondResult := aString; Exit; end;

  aFirstResult  := AnsiLeftStr ( aString, iSplitCharPos - 1 );
  aSecondResult := AnsiRightStr( aString, Length( aString ) - iSplitCharPos );
end;

function CroppedString( const aString : Ansistring; aLength : Word) : Ansistring;
begin
  CroppedString := aString;
  Delete( CroppedString, aLength + 1 ,255 );
end;

procedure Pad( var aString : AnsiString; aLength : Byte; aPadChar : Char = ' ' );
var iOldLength : Word;
begin
  iOldLength := Length( aString );
  SetLength( aString, aLength );
  if aLength > iOldLength then
    FillChar( aString[ iOldLength + 1 ], aLength - iOldLength, aPadChar );
end;

function Padded( const aString : AnsiString; aLength : Byte; aPadChar : Char = ' ' ) : AnsiString;
begin
  Padded := aString;
  Pad( Padded, aLength, aPadChar );
end;

function CapLet(const s : Ansistring):Ansistring;
begin
  Exit( Capitalized( s ) );
end;

function Parameter( const aString : AnsiString; aNumber : Byte; const aParamChar: AnsiString ) : AnsiString;
var iPos : Integer;
begin
  if Length( aParamChar ) = 1 then Parameter := ExtractDelimited( aNumber, aString, [ aParamChar[1] ]) else
  if Length( aParamChar ) = 2 then
  begin
    iPos := Pos( aParamChar[1], aString ) + 1;
    Parameter := ExtractSubStr( aString, iPos, [ aParamChar[2] ] )
  end
  else
  begin
    iPos := Pos( aParamChar[1], aString ) + 1;
    Parameter := ExtractDelimited( aNumber, ExtractSubStr( aString, iPos, [ aParamChar[3] ] ), [ aParamChar[2], aParamChar[3] ]);
  end;
end;

function StrToBool( const aString : AnsiString ) : Boolean;
begin
  if length( aString ) < 1 then Exit( False );
  Exit( aString [1] in ['t','T','y','Y'] );
end;


function Dice( aNumber : DWord; aSides : DWord ) : DWord;
var iCount : DWord;
begin
  Dice := 0;
  if aNumber * aSides = 0 then Exit(0);
  if aSides = 1 then Exit( aNumber );
  for iCount := 1 to aNumber do
     Dice += Random( aSides ) + 1;
end;

function TimeStamp : string;
begin
  Exit(DateTimeToStr(Now));
end;

function BoolToStr( const aBoolean : boolean ) : string;
begin
  if aBoolean then Exit('TRUE') else exit('FALSE');
end;

function ReadVersion(const aFileName : string) : string;
var iTextFile : Text;
begin
  {$PUSH}
  {$I-}
  Assign(iTextFile,aFileName);
  Reset(iTextFile);
  Readln(iTextFile,ReadVersion);
  Close(iTextFile);
  {$POP} {restore $I}
  if IOResult <> 0 then raise EException.Create('Version file '+aFileName+' not found!');
end;

{ EException }

constructor EException.Create(const aMessage: AnsiString);
begin
  inherited Create( aMessage );
end;

constructor EException.Create(const aMessage: AnsiString; const aParams: array of const);
begin
  inherited Create( VFormat( aMessage, aParams ) );
end;

begin
  Randomize;
end.

//* 23.11.2004 Removed a lot of useless stuff.
// 2006-NOV-09 Removed TrimString (use SysUtils.Trim) and added new Split
//*

