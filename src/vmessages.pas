{$INCLUDE valkyrie.inc}
unit vmessages;
interface
uses vgenerics;

type TMessageBuffer         = specialize TGRingBuffer< Ansistring >;
     TMessagesMoreEvent     = procedure of object;

type TMessageHighlight = record
    Wildcard : Ansistring;
    Letter   : Char;
  end;

type TMessageHighlightArray = specialize TGArray<TMessageHighlight>;

type TMessages = class
  constructor Create( aVisible : DWord; aLength : DWord; aOnMore : TMessagesMoreEvent; aBufferSize : Word = 1000 );
  procedure Add( const aMessage : Ansistring );
  procedure Update;
  procedure Clear;
  procedure Reset;
  function Size : DWord;
  procedure AddHighlightCallback( aKey, aValue : Variant );
  destructor Destroy; override;
protected
  FHighlights  : TMessageHighlightArray;
  FContent     : TMessageBuffer;
  FActive      : DWord;
  FOnMore      : TMessagesMoreEvent;
  FVisible     : DWord;
  FLength      : DWord;
public
  property Active  : DWord          read FActive;
  property Content : TMessageBuffer read FContent;
  property Visible : DWord          read FVisible;
end;

implementation

uses sysutils, strutils, vtig;

constructor TMessages.Create ( aVisible : DWord; aLength : DWord; aOnMore : TMessagesMoreEvent; aBufferSize : Word = 1000 ) ;
begin
  FOnMore     := aOnMore;
  FActive     := 0;
  FContent    := TMessageBuffer.Create( aBufferSize );
  FHighlights := TMessageHighlightArray.Create;
  FVisible    := aVisible;
  FLength     := aLength;
end;

procedure TMessages.Add( const aMessage : Ansistring );
var iPending : Ansistring;
    iPrefix  : Ansistring;
    iLength  : Integer;
    iBreak   : Integer;
    iLetter  : Char;

  function FindBreak( aFragment : AnsiString; aMax : Integer ) : Integer;
  var iPos : Integer;
  begin
    FindBreak := 0;
    iPos := 0;
    while aMax > 0 do
    begin
      Inc( iPos );
      if iPos >= Length( aFragment ) then Exit( Length( aFragment ) + 1 );
           if aFragment[iPos] = '{'    then begin Inc( iPos ); Continue end
      else if aFragment[iPos] = '}'    then Continue
      else if aFragment[iPos] in [' '] then FindBreak := iPos;
      Dec( aMax );
    end;
  end;

  function LetterEscape( aString : AnsiString ) : AnsiString;
  begin
    if ( aString <> '' ) and ( iLetter <> ' ' )
      then Exit( '{'+iLetter+aString+'}' )
      else Exit( aString );
  end;


  procedure NewLine( aContent : Ansistring );
  begin
    if ( FActive = FVisible ) then
    begin
      if Assigned( FOnMore ) then FOnMore;
      FActive := 0;
    end;
    Inc( FActive );
    FContent.PushBack( LetterEscape( aContent ) );
  end;

begin
  if aMessage = '' then Exit;
  iLetter  := ' ';
  if FHighlights.Size > 0 then
  for iBreak := 0 to FHighlights.Size-1 do
    with FHighlights[iBreak] do
      if IsWild(aMessage,FHighlights[iBreak].Wildcard,False) then
        iLetter := FHighlights[iBreak].Letter;

  iPending := aMessage;
  iLength  := 0;
  while iPending <> '' do
  begin
    iLength := FLength;
    if ( FActive > 0 ) and ( Length( FContent.Back ) > 0 ) then
      iLength -= ( VTIG_Length( FContent.Back ) + 1 );
    iBreak := FindBreak( iPending, iLength );
    if iBreak = 0 then
    begin
      if iLength < FLength then
      begin
        NewLine('');
        Continue;
      end
      else
        iBreak := iLength - 1;
    end;

    iPrefix := Copy( iPending, 1, iBreak - 1 );
    if ( Length( iPending ) - iBreak ) > 0
      then iPending := Copy( iPending, iBreak + 1, Length( iPending ) - iBreak )
      else iPending := '';

    if FActive = 0 then
      NewLine( iPrefix )
    else
      if Length( FContent[-1] ) = 0
        then FContent[-1] := FContent[-1] + LetterEscape( iPrefix )
        else FContent[-1] := FContent[-1] + ' ' + LetterEscape( iPrefix );

    if iPending <> '' then
      NewLine('');
  end;
end;

procedure TMessages.Update;
begin
  FActive := 0;
end;

procedure TMessages.Clear;
begin
  FContent.Clear;
  FActive := 0;
end;

procedure TMessages.Reset;
var iCount : Integer;
begin
  for iCount := 1 to FVisible do
    FContent.PushBack( '' );
end;

function TMessages.Size : DWord;
begin
  Exit( FContent.Size );
end;

destructor TMessages.Destroy;
begin
  FreeAndNil( FHighlights );
  FreeAndNil( FContent );
  inherited Destroy;
end;

procedure TMessages.AddHighlightCallback ( aKey, aValue : Variant ) ;
var iHighlight : TMessageHighlight;
    iColor     : Byte;
const Colors = 'bgcrmyldBGCRMYL';
begin
  iColor := aValue;
  if (iColor > 0) and (iColor < 16) then
  begin
    iHighlight.Letter   := Colors[iColor];
    iHighlight.Wildcard := aKey;
    FHighlights.Push( iHighlight );
  end;
end;

end.

