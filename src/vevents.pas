// @abstract(Event handling unit for GenRogue Core)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
//
// Implements all event handling classes, that is @link(TEvent)
// and @link(TEventController). Implements global variables
// @link(Events) and @link(EventTimer).
{$INCLUDE valkyrie.inc}
{$H-}
unit vevents;
interface
uses Classes, vnode, vutil, vgenerics;

type TMessage = packed record
  MSGID : Cardinal;
end;

// Main event class, stored by @link(TEventController).
type TEvent   = packed record
  // The time the event will be executed.
  Sequence : QWord;
  // @link(TUID) of the target.
  Target   : TUID;
  // TMessage size
  MSGSize  : Word;
  // Carried @link(TMessage).
  MSG      : TMessage;
end;

type PEvent = ^TEvent;


type TEventHeapQueue = specialize TGHeap<PEvent>;

// A specialization to for THeapQueue tailored to handling events.
type

{ TEventQueue }

 TEventQueue = class(TEventHeapQueue)
   // Creates a HeapQueue and registers comparision.
   constructor Create;
   // Marks all events pointing to nUID as invalid.
   procedure RemoveUID(nUID : TUID);
end;

// @link(TEvent) holder and manager. Holds all events in a priority queue.
// Used as a singleton @link(Events).
type

{ TEventController }

 TEventController = class(TNode)
  // Standard constructor.
  constructor Create; override;
  // Standard destructor, frees all unexecuted events.
  destructor Destroy; override;
  // Adding an event to the queue. Timeleft calculates
  // execution time based on @link(EventTimer). MSG is copied.
  procedure AddEvent(TimeLeft : QWord; nTarget : TUID; aMSGSize : Word; var MSG );
  // Shorthand for simple events
  procedure AddEvent(TimeLeft : QWord; nTarget : TUID; MSG : Cardinal );
  // Removes all events with the given @link(TUID)
  // from the priority queue.
  procedure RemoveUID(nUID : TUID);
  // Advances time by 1.
  procedure AdvanceTime;
  // Handles a single event if due. Returns if there was any event handled.
  function HandleEvent : Boolean;
  // Stream constructor, reads UID, and ID from stream, should be overriden.
  constructor CreateFromStream( Stream : TStream ); override;
  // Write Node to stream (UID and ID) should be overriden.
  procedure WriteToStream( Stream : TStream ); override;
  // Returns amount of events
  function GetSize : Integer;
  private
  // First event in queue.
  FQueue : TEventQueue;
  // Global variable holding the current time. Queried to
  // get knowledge wether an event should be executed.
  FTimer : QWord;
  private
  // Dispatches event and destroys it. Returns if event was valid.
  function Resolve( aEvent : PEvent ) : Boolean;
  public
  property Size : Integer read GetSize;
end;

// Singleton @link(TEventController) class. Needs to be initialized.
const Events   : TEventController = nil;

implementation

uses SysUtils, vuid;

function EventCompare( const Item1, Item2: PEvent ): Integer;
begin
       if Item1^.Sequence < Item2^.Sequence then Exit(1)
  else if Item1^.Sequence > Item2^.Sequence then Exit(-1)
  else Exit(0);
end;

constructor TEventQueue.Create;
begin
  inherited Create(@EventCompare);
end;

// Marks all events pointing to nUID as invalid.
procedure TEventQueue.RemoveUID(nUID : TUID);
var Event : PEvent;
begin
  for Event in Self do
    if Event^.Target = nUID then
      Event^.Target := 0;
end;

constructor TEventController.Create;
begin
  inherited Create;
  FQueue := TEventQueue.Create;
end;

procedure TEventController.RemoveUID(nUID : TUID);
begin
  FQueue.RemoveUID(nUID);
end;

procedure TEventController.AdvanceTime;
begin
  Inc( FTimer );
end;

function TEventController.HandleEvent: Boolean;
var iEvent : PEvent;
begin
  Result := False;
  while (not FQueue.isEmpty) and (FQueue.Top^.Sequence <= FTimer) do
  begin
    iEvent := FQueue.Pop;
    if Resolve( iEvent ) then Exit( True );
  end;
end;


constructor TEventController.CreateFromStream(Stream: TStream);
var iCount, iEvents : DWord;
    iSize           : Word;
    iEvent          : PEvent;
begin
  inherited CreateFromStream(Stream);
  FTimer := Stream.ReadQWord;
  FQueue := TEventQueue.Create;
  iEvents := Stream.ReadDWord;
  for iCount := 1 to iEvents do
  begin
    iSize  := Stream.ReadWord;
    if iSize = 0 then Break;
    iEvent := GetMem( SizeOf( TEvent ) - SizeOf( Cardinal ) + iSize );
    iEvent^.MSGSize  := iSize;
    iEvent^.Sequence := Stream.ReadQWord;
    iEvent^.Target   := Stream.ReadQWord;
    Stream.Read( iEvent^.MSG, iEvent^.MSGSize );
    FQueue.Insert( iEvent );
  end;
  Stream.ReadWord;
end;

procedure TEventController.WriteToStream(Stream: TStream);
var iEvent : PEvent;
begin
  inherited WriteToStream(Stream);
  Stream.WriteQWord( FTimer );
  Stream.WriteDWord( FQueue.Size );
  for iEvent in FQueue do
  if iEvent^.Target <> 0 then
  begin
    Stream.WriteWord( iEvent^.MSGSize );
    Stream.WriteQWord( iEvent^.Sequence );
    Stream.WriteQWord( iEvent^.Target );
    Stream.Write( iEvent^.MSG, iEvent^.MSGSize );
  end;
  Stream.WriteWord( 0 );
end;

function TEventController.GetSize: Integer;
begin
  Exit( FQueue.Size );
end;

function TEventController.Resolve(aEvent: PEvent): Boolean;
var iTarget : TNode;
begin
  if aEvent^.Target <> 0 then
  begin
    iTarget := UIDs.Get( aEvent^.Target );
    if iTarget <> nil then
    begin
      iTarget.Dispatch( aEvent^.MSG );
      Exit( True );
    end;
  end;
  FreeMem( aEvent );
  Exit( False );
end;


procedure TEventController.AddEvent(TimeLeft : QWord; nTarget : TUID; aMSGSize : Word; var MSG);
var iEvent : PEvent;
begin
  iEvent := GetMem( SizeOf( TEvent ) - SizeOf( Cardinal ) + SizeOf( MSG ) );
  iEvent^.Sequence := FTimer + TimeLeft;
  iEvent^.Target   := nTarget;
  iEvent^.MSGSize  := aMSGSize;
  System.Move( MSG, iEvent^.MSG, aMSGSize );
  FQueue.Insert( iEvent );
end;

procedure TEventController.AddEvent(TimeLeft: QWord; nTarget: TUID; MSG: Cardinal);
var iMessage : TMessage;
begin
  iMessage.MSGID := MSG;
  AddEvent( TimeLeft, nTarget, SizeOf( iMessage ), iMessage );
end;

destructor TEventController.Destroy;
var iEvent : PEvent;
begin
  for iEvent in FQueue do
    FreeMem( iEvent, SizeOf( TEvent ) - SizeOf( Cardinal ) + iEvent^.MSGSize );
  FreeAndNil(FQueue);
  inherited Destroy;
end;

end.

//* Created 22.08.2003
