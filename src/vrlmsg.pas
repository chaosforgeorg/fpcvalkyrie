{$INCLUDE valkyrie.inc}
unit vrlmsg;
interface

uses vuitypes, vgenerics, viotypes;

type TRLMsgMoreEvent = function : Boolean of object;
     TRLChunkify     = function( const aString : AnsiString; aStart : Integer; aColor : TIOColor ) : TUIChunkBuffer of object;
type TRLMessageHighlight = record
    Wildcard : Ansistring;
    Color    : TUIColor;
  end;

type TRLHighlightsArray = specialize TGArray<TRLMessageHighlight>;


type TRLMessages = class
  // Creates a new message system. Pos and Size are used to place the Messages
  // on the textmode screen. BufferSize determines how many messages will be
  // stored by the system.
  constructor Create( aVisibleCount : DWord; aOnMore : TRLMsgMoreEvent; aChunkify : TRLChunkify; aBufferSize : Word = 1000 );
  // Adds a new message to the system. By default the message will be active.
  procedure Add( const aMessage : Ansistring );
  // Handle more case
  procedure More;
  // Updates the messages -- all messages are marked as "read" - inactive.
  // Should be run immidately AFTER Updating the screen.
  procedure Update;
  // Destroy all messages and cleanup
  procedure Clear;
  // Push empty lines in the visible amount
  procedure Reset;
  // Destroys last added message line.
  procedure Pop;
  // Frees memory of the message system.
  destructor Destroy; override;
  // Returns amount of stored message lines.
  function Size : DWord;
  // add pending
  procedure AddPending;
  // Add highlight, Variants used for callback support
  procedure AddHighlightCallback( aKey, aValue : Variant );
protected
  FHighlights  : TRLHighlightsArray;
  //
  FContent     : TUIChunkBuffer;
  // Number of highlights.
  FPending     : TUIChunkBuffer;
   // Number of highlights.
  FHighCount   : Word;
  // Holds the AMOUNT of active message lines
  FActive      : Word;
  //
  FOnMore      : TRLMsgMoreEvent;
  FChunkify    : TRLChunkify;

  FScroll      : DWord;
  FCount       : DWord;
  FVisibleCount: DWord;
public
  property OnMoreEvent : TRLMsgMoreEvent write FOnMore;
  property Active : Word read FActive write FActive;
  property Content : TUIChunkBuffer read FContent;
  property Scroll : DWord read FScroll;
  property VisibleCount : DWord read FVisibleCount;
end;

implementation

uses math, sysutils, strutils, vmath;

constructor TRLMessages.Create ( aVisibleCount : DWord; aOnMore : TRLMsgMoreEvent;
  aChunkify : TRLChunkify; aBufferSize : Word ) ;
begin
  FOnMore       := aOnMore;
  FChunkify     := aChunkify;
  FActive       := 0;
  FContent      := TUIChunkBuffer.Create( aBufferSize );
  FPending      := nil;
  FHighlights   := TRLHighlightsArray.Create;
  FScroll       := 0;
  FVisibleCount := aVisibleCount;
  FCount        := 0;
  FScroll       := 0;
end;

procedure TRLMessages.Add ( const aMessage : Ansistring ) ;
var iCurrent : Integer;
    iCount   : Integer;
    iChunk   : TUIChunk;
    iColor   : TUIColor;
begin
  // currently let's assume that more is not active
  iCurrent := 0;
  if (FActive > 0) and (High(FContent.Back) >= 0) then
  begin
    iChunk   := FContent.Back[ High(FContent.Back) ];
    iCurrent := Length( iChunk.Content ) + iChunk.Position.x+1;
  end;

  iColor := ColorNone;
  if FHighlights.Size > 0 then
  for iCount := 0 to FHighlights.Size-1 do
    with FHighlights[iCount] do
      if IsWild(aMessage,FHighlights[iCount].Wildcard,False) then
        iColor := FHighlights[iCount].Color;

  FPending := FChunkify( aMessage, iCurrent, iColor );
  if FPending = nil then Exit;

  // handle the left space
  if iCurrent > 0 then
  begin
    FContent[-1] := ChunkListAppend( FContent[-1], FPending.Front );
    if FPending.Size <= 1 then
    begin
      FreeAndNil( FPending );
      Exit;
    end;
    FPending.PopFront;
    iCurrent := 0;
  end;

  AddPending;
  FScroll := Clamp( FCount, 0, Max( FCount-FVisibleCount, 0 ) );
end;

procedure TRLMessages.More;
begin
  if Assigned( FOnMore ) then FOnMore;
end;

procedure TRLMessages.Update;
begin
  FActive := 0;
  AddPending;
end;

procedure TRLMessages.Clear;
begin
  FContent.Clear;
  FActive := 0;
end;

procedure TRLMessages.Reset;
var iCount : Integer;
begin
  for iCount := 1 to FVisibleCount do
    FContent.PushBack( nil );
end;

procedure TRLMessages.Pop;
begin
  if FContent.Size > 0 then FContent.PopBack;
  if FActive > 0 then Dec( FActive );
end;

destructor TRLMessages.Destroy;
begin
  FreeAndNil( FHighlights );
  FreeAndNil( FContent );
  inherited Destroy;
end;

function TRLMessages.Size : DWord;
begin
  Exit( FContent.Size );
end;

procedure TRLMessages.AddPending;
begin
  if FPending = nil then Exit;

  while (FActive < FVisibleCount) and (FPending.Size > 0) do
  begin
    FContent.PushBack( FPending.Front );
    FPending.PopFront;
    Inc(FActive);
  end;
  FCount := FContent.Size;

  if FPending.Size = 0 then
    FreeAndNil( FPending )
  else
    More;
end;

procedure TRLMessages.AddHighlightCallback ( aKey, aValue : Variant ) ;
var iHighlight : TRLMessageHighlight;
begin
  iHighlight.Color    := aValue;
  iHighlight.Wildcard := aKey;
  FHighlights.Push( iHighlight );
end;

end.

