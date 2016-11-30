{$INCLUDE valkyrie.inc}
unit vuitypes;
interface
uses Classes, SysUtils, vgenerics, viotypes;

type
  TUIString        = AnsiString;
  TUIChar          = Char;
  TUICharSet       = set of Char;
  TUIStringArray   = specialize TGArray<AnsiString>;
  TUIStringBuffer  = specialize TGRingBuffer<AnsiString>;
  TUIColor         = TIOColor;
  TUIRect          = TIORect;
  TUIPoint         = TIOPoint;
  TUIOrientation   = ( VORIENT_HORIZONTAL, VORIENT_VERTICAL );

  TUIChunk         = record
    Position : TUIPoint;
    Color    : TUIColor;
    Content  : TUIString;
  end;
  TUIChunkList      = array of TUIChunk;
  TUIChunkListArray = specialize TGArray< TUIChunkList >;
  TUIChunkBuffer    = specialize TGRingBuffer< TUIChunkList >;

function StripEncoding( const aUIString : TUIString ) : TUIString;
function ChunkListAppend( aDestination : TUIChunkList; const aSource : TUIChunkList ) : TUIChunkList;
function ChunkListToString( const aList : TUIChunkList ) : TUIString;
function TextFileToUIStringArray( const aPath : AnsiString ) : TUIStringArray;

implementation

// TODO: Optimize with ExtractString
function StripEncoding( const aUIString : TUIString ) : TUIString;
var iLength,iPos : DWord;
begin
  iLength := system.Length(aUIString);
  if iLength = 0 then Exit('');
  iPos := 0;
  StripEncoding := '';
  repeat
    Inc(iPos);
    if aUIString[iPos] = '@' then
    begin
      Inc(iPos);
      if iPos > iLength then Exit;
      case aUIString[iPos] of
        '@' : StripEncoding += '@';
      end;
    end
    else StripEncoding += aUIString[iPos];
  until iPos >= iLength;
end;

function ChunkListAppend ( aDestination : TUIChunkList;
  const aSource : TUIChunkList ) : TUIChunkList;
var iCount : Integer;
    iPos   : Integer;
begin
  if High( aSource ) < 0 then Exit( aDestination );
  iPos := High( aDestination )+1;
  SetLength( aDestination, Length( aDestination ) + Length( aSource ) );
  for iCount := 0 to High( aSource ) do
    aDestination[ iPos + iCount ] := aSource[ iCount ];
  Exit( aDestination );
end;

function ChunkListToString ( const aList : TUIChunkList ) : TUIString;
var iCount : Integer;
begin
  Result := '';
  for iCount := 0 to High( aList ) do
    Result += aList[iCount].Content+' ';
end;

function TextFileToUIStringArray ( const aPath : AnsiString ) : TUIStringArray;
var T : Text;
    S : AnsiString;
begin
  Result := TUIStringArray.Create;
  AssignFile( T, aPath );
  Reset( T );
  repeat
    Readln( T, S );
    Result.Push( S );
  until EOF( T );
  Close( T );
end;

end.

