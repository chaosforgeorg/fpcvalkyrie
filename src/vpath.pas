{$INCLUDE valkyrie.inc}
// @abstract(Pathfinding for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(Feb 19, 2010)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-10-14 19:45:46 +0200 (Tue, 14 Oct 2008) $)
//
// Gathers some useful functions for roguelike development in Valkyrie.
// As for the current state it's experimental.
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

unit vpath;
interface

uses Classes, SysUtils, vgenerics, vrltools;

{$M-}
type IPathQuery = interface['vpath.pathquery']
  function MoveCost( const Start, Stop : TCoord2D ) : Single;
  function CostEstimate( const Start, Stop : TCoord2D ) : Single;
  function passableCoord( const Coord : TCoord2D ) : boolean;
end;

type

{ TAStarNode }

TAStarNode = class
    Coord         : TCoord2D;
    CostFromStart : Single;
    CostToGoal    : Single;
    CostTotal     : Single;
    Child         : TAStarNode;
    Parent        : TAStarNode;
    constructor Create;
    destructor Destroy; override;
  end;

type TAStarNodeQueue  = specialize TGObjectHeap< TAStarNode >;
     TAStarNodeVector = specialize TGObjectArray< TAStarNode >;

type

{ TPathfinder }

TPathfinder = class
  constructor Create( newMap : IPathQuery );
  procedure ChangeSource( newMap : IPathQuery );
  function Run( Position : TCoord2D; Target : TCoord2D; CutOff : DWord; Maximum : DWord = 0 ) : Boolean;
  function QuickRun( Position : TCoord2D; Target : TCoord2D; var aResult : TCoord2D; CutOff : DWord; Maximum : DWord = 0 ) : Boolean;
  function Found : Boolean;
  procedure Clear;
  destructor Destroy; override;
(* *)public
  FClosed   : TAStarNodeVector;
  FOpen     : TAStarNodeQueue;
protected
  FStart    : TAStarNode;
  FStop     : TAStarNode;
  FTarget   : TCoord2D;
  FPosition : TCoord2D;
  FMap      : IPathQuery;
  FMaxNodes : DWord;
protected
  function NewNode : TAStarNode;
  procedure DeleteNode( Node : TAStarNode );
public
  property Target   : TCoord2D read FTarget;
  property Position : TCoord2D read FPosition;
  property Start    : TAStarNode read FStart write FStart;
  property Stop     : TAStarNode read FStop;
  property MaxNodes : DWord read FMaxNodes;
end;

var AStarCount : DWord = 0;

implementation

uses math;

const VNOT_FOUND : DWord = $FFFFFFFF;

{ TPathfinder }


constructor TAStarNode.Create;
begin
  Inc( AStarCount );
end;

destructor TAStarNode.Destroy;
begin
  Dec( AStarCount );
end;

function CompareNode( const NodeA, NodeB : TAStarNode ) : Integer;
begin
  Exit( Sign( NodeB.CostTotal - NodeA.CostTotal ) );
end;

constructor TPathfinder.Create(newMap: IPathQuery);
begin
  FMaxNodes := 0;
  FMap := newMap;
  FClosed := TAStarNodeVector.Create( False );
  FOpen   := TAStarNodeQueue.Create( @CompareNode, False );
  FStart    := nil;
  FStop     := nil;
end;

procedure TPathfinder.ChangeSource(newMap: IPathQuery);
begin
  FMap := newMap;
end;

function TPathfinder.Run(Position: TCoord2D; Target: TCoord2D; CutOff : DWord; Maximum : DWord = 0 ): Boolean;
var Node, NodeParent : TAStarNode;
    MinNode          : TAStarNode;
    Area             : TArea;
    Coord            : TCoord2D;
    Index            : DWord;
    OpenIndex        : DWord;
    ClosedIndex      : DWord;
    Cost             : Single;
    Count            : DWord;
begin
  Clear;
  Count := 0;
  if Maximum = 0 then Maximum := Cutoff;

  FStart               := NewNode;
  FStart.Coord         := Position;
  FStart.CostFromStart := 0;
  FStart.CostToGoal    := FMap.CostEstimate(Position,Target);
  FStart.CostTotal     := FStart.CostToGoal;
  FOpen.Insert( FStart );
  MinNode := FStart;

  while not FOpen.IsEmpty do
  begin
    Inc( Count );
    FMaxNodes := Max( FMaxNodes, AStarCount );
    Node := FOpen.Pop;

    if (Node.Coord = Target) or (Count > Maximum) or ((Count > Cutoff) and (MinNode.Parent <> nil))then
    begin
      FOpen.Insert( Node );
      if Node.Coord <> Target then Node := MinNode;
      FStop      := Node;
      NodeParent := Node.Parent;
      if NodeParent <> nil then
      repeat
        NodeParent.Child := Node;
        Node       := NodeParent;
        NodeParent := NodeParent.Parent;
      until Node = FStart
      else Exit( False );
      Exit( True );
    end;
    NodeParent := Node;
    Area := NewArea( NodeParent.Coord, 1 );

    for Coord in Area do
    begin
      if Coord = NodeParent.Coord then Continue;
      if not FMap.passableCoord( Coord ) then Continue;
      Cost  := NodeParent.CostFromStart + FMap.MoveCost( NodeParent.Coord, Coord );

      Node        := nil;
      OpenIndex   := VNOT_FOUND;
      ClosedIndex := VNOT_FOUND;

      if FOpen.Size > 0 then
      for Index := 0 to FOpen.Size - 1 do
      begin
        if FOpen[ Index ].Coord = Coord then
        begin
          OpenIndex := Index;
          Node := FOpen[ Index ];
          Break;
        end;
      end;

      if (OpenIndex <> VNOT_FOUND) and (FOpen[ OpenIndex ].CostFromStart <= Cost) then
         Continue;

      if FClosed.Size > 0 then
      for Index := 0 to FClosed.Size - 1 do
      begin
        if FClosed[ Index ].Coord = Coord then
        begin
          ClosedIndex := Index;
          Node := FClosed[ Index ];
          Break;
        end;
      end;

      if (ClosedIndex <> VNOT_FOUND) and (FClosed[ ClosedIndex ].CostFromStart <= Cost) then
         Continue;

      if Node = nil then Node := NewNode;
      Node.Coord         := Coord;
      Node.Parent        := NodeParent;
      Node.CostFromStart := Cost;
      Node.CostToGoal    := FMap.CostEstimate(Coord,Target);
      Node.CostTotal     := Node.CostToGoal + Node.CostFromStart;

      if Node.CostToGoal < MinNode.CostToGoal then MinNode := Node;

      if ClosedIndex <> VNOT_FOUND then
        if ClosedIndex = FClosed.Size-1
          then FClosed.Pop
          else FClosed[ ClosedIndex ] := FClosed.Pop;

      if OpenIndex <> VNOT_FOUND then
        FOpen.Delete(OpenIndex);

      FOpen.Insert( Node );
    end;

    FClosed.Push( NodeParent );
  end;
  Exit( False );
end;

function TPathfinder.QuickRun ( Position : TCoord2D; Target : TCoord2D;
  var aResult : TCoord2D; CutOff : DWord; Maximum : DWord ) : Boolean;
begin
  QuickRun := Run( Position, Target, CutOff, Maximum );
  if (not QuickRun) or (not Found) then Exit( False );
  FStart := FStart.Child;
  if FStart = nil then Exit( False );
  aResult := FStart.Coord;
end;

function TPathfinder.Found: Boolean;
begin
  Exit( FStop <> nil );
end;

procedure TPathfinder.Clear;
var Index : DWord;
begin
  if FClosed.Size > 0 then
  begin
    for Index := 0 to FClosed.Size-1 do DeleteNode( FClosed[ Index ] );
    FClosed.Reset;
  end;

  if FOpen.Size > 0 then
  begin
    for Index := 0 to FOpen.Size-1 do DeleteNode( FOpen[ Index ] );
    FOpen.Clear;
  end;

  FStart := nil;
  FStop  := nil;
end;

destructor TPathfinder.Destroy;
begin
  Clear;
  FreeAndNil( FClosed );
  FreeAndNil( FOpen );
end;

function TPathfinder.NewNode: TAStarNode;
begin
  NewNode        := TAStarNode.Create;
  NewNode.Parent := nil;
  NewNode.Child  := nil;
end;

procedure TPathfinder.DeleteNode( Node: TAStarNode);
begin
  FreeAndNil( Node );
end;

{ TAStarNode }


end.

