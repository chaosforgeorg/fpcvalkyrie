{$INCLUDE valkyrie.inc}
// @abstract(Unique Identification class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
// @lastmod(Jan 14, 2006)
//
// Contains class and singleton for managing Unique Identification
// Numbers.
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

unit vuid;
interface
uses Classes, vutil, vnode, vsystem;

type TUIDStorePolicy = ( UIDReturnZero, UIDThrowException, UIDGrow );

// Manager class for UIDs. Used through the singleton @link(UIDs).
type TUIDStore = class(TSystem)
       // Standard constructor.
       constructor Create( aSize : DWord = $FFFF); reintroduce;
       // UID lookup. Returns the @link(TNode) referenced by given UID.
       function    Get( aUID : TUID ) : TNode;
       // Marks the UID as invalid
       procedure   Remove( aUID : TUID ); reintroduce;
       // Registers the Node with given UID.
       procedure   Register( aNode : TNode; aUID : TUID);
       // Registers the Node, returns UID (save it!).
       function    Register( aNode : TNode ) : TUID;
       // Returns a unused UID number, marks it as used!
       function    GetFreeID : TUID;
       // Standard destructor.
       destructor  Destroy; override;
       // Property for Get
       property UIDs[aUID : TUID] : TNode read Get; default;
       // Stream constructor, reads ONLY the current Policy, UID and Size and
       // Initial parameters!
       constructor CreateFromStream( Stream : TStream ); override;
       // Writes to stream the current *settings* of the UIDstore
       procedure WriteToStream( Stream : TStream ); override;
     private
       FPolicy  : TUIDStorePolicy;
       FIDCount : TUID;
       FData    : array of TNode;
       FSize    : DWord;
       FInitial : DWord;
     private
       // Grows the UID store
       procedure Grow;
     public
       property Policy : TUIDStorePolicy read FPolicy write FPolicy;
       property Count : QWord read FIDCount;
       property Size : DWord read FSize;
     end;

// Singleton for using @link(TUIDStore).
const UIDs : TUIDStore = nil;

implementation

constructor TUIDStore.Create( aSize : DWord );
begin
  inherited Create;
  FSize    := aSize;
  FInitial := aSize;
  FPolicy  := UIDGrow;
  SetLength( FData, aSize );
  FillChar( FData[0], aSize*SizeOf(TNode),0 );
  FIDCount := 0;
end;

function TUIDStore.Get(aUID : TUID) : TNode;
begin
  Get := FData[aUID];
end;

procedure TUIDStore.Remove( aUID : TUID );
begin
  FData[ aUID ] := nil;
end;

procedure TUIDStore.Register( aNode : TNode; aUID : TUID );
begin
  FData[ aUID ] := aNode;
end;

function TUIDStore.Register( aNode : TNode ) : TUID;
begin
  Register := GetFreeID;
  FData[ Register ] := aNode;
end;

function TUIDStore.GetFreeID : TUID;
begin
  Inc( FIDCount );
  if FIDCount = FSize then
  case FPolicy of
    UIDReturnZero     : begin Dec( FIDCount ); Exit( 0 ); end;
    UIDThrowException : raise EException.Create('TUIDStore : Ran out of free UID''s!');
    UIDGrow           : Grow;
  end;
  Exit( FIDCount );
end;

destructor TUIDStore.Destroy;
begin
  SetLength( FData, 0 );
  inherited Destroy;
end;

constructor TUIDStore.CreateFromStream ( Stream : TStream ) ;
begin
  inherited Create;
  FPolicy  := TUIDStorePolicy( Stream.ReadByte );
  FIDCount := Stream.ReadQWord;
  FSize    := Stream.ReadDWord;
  FInitial := Stream.ReadDWord;

  SetLength( FData, FSize );
  FillChar( FData[0], FSize*SizeOf(TNode), 0 );
end;

procedure TUIDStore.WriteToStream ( Stream : TStream ) ;
begin
  Stream.WriteByte( Byte( FPolicy ) );
  Stream.WriteQWord( FIDCount );
  Stream.WriteDWord( FSize );
  Stream.WriteDWord( FInitial );
end;

procedure TUIDStore.Grow;
var NewSize : DWord;
begin
  NewSize := FSize + FInitial;
  SetLength( FData, NewSize );
  FillChar( FData[FSize], FInitial*SizeOf(TNode), 0 );
  FSize := NewSize;
end;

end.
