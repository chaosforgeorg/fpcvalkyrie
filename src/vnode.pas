{$INCLUDE valkyrie.inc}
// @abstract(Node class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-01-14 22:16:41 +0100 (Mon, 14 Jan 2008) $)
//
// @link(TNode) is core class from which other classes inherit.
// It implements a tree-like structure. Also, each node
// has an unique identifier, represented by @link(TUID).
//
// This unit also implements the two Valkyrie base classes :
// TVObject and TVClass.
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
//
// @preformatted(
// TODO: Check wether a Node can dispose of itself via Self.Done.
// TODO: Implement generic Save/Load via streams.
// )

unit vnode;
interface
uses Classes, vmsg, vutil, vdebug, vluatype, vlualibrary;

// The most generic of Valkyrie objects. Implements only the error
// handling functions. It is recommended that all Valkyrie classes
// inherit at least this class.
type

{ TVObject }

TVObject = class( TObject )
     // TVObject Interface for @link(grdebug.Log).
     procedure   Log( const aLogString : Ansistring ); virtual;
     // TVObject Interface for @link(grdebug.Log).
     procedure   Log( aLevel : TLogLevel; const aLogString : Ansistring ); virtual;
     // TVObject Interface for @link(grdebug.Log), VFormat version.
     procedure   Log( const aLogString : Ansistring; const aParam : array of Const );
     // TVObject Interface for @link(grdebug.Log), VFormat version.
     procedure   Log( aLevel : TLogLevel; const aLogString       : Ansistring; const aParam : array of Const );
     // Returns wether the object has a parent -- in case of TVObject it's always false
     function hasParent : boolean; virtual;
     // Returns wether the object has a child -- in case of TVObject it's always false
     function hasChild : boolean; virtual;
     // Returns wether the object is a TVNode
     function isNode : boolean; virtual;
     // Stream constructor, reads UID, and ID from stream, should be overriden.
     constructor CreateFromStream( Stream : TStream ); virtual;
     // Write Node to stream (UID and ID) should be overriden.
     procedure WriteToStream( Stream : TStream ); virtual;
  end;

type TVObjectClass = class of TVObject;

type TNode = class;
type TNodeClass = class of TNode;

// Enumerator for nodes. Implemented as a generic so it may be reused
// For a node that implements TNode.

  { TGNodeEnumerator }

  generic TGNodeEnumerator<T> = object
  protected
    // We store the parent node for the case if during iteration the child node
    // changes it's parent.
    FParent   : TNode;
    // Current node, returned by the iterator.
    FCurrent  : TNode;
    // Next node, held separately so we can go on smoothly event if the current
    // node changes it's parent.
    FNext     : TNode;
    // Returns current node as T. No checking is done!
    function GetCurrent: T;
  public
    // Creates the iterator
    constructor Create( Parent : TNode );
    // Moves the iterator and returns whether current if valid
    function MoveNext : Boolean;
    // Returns current node
    property Current : T read GetCurrent;
  end;

  // Specialization of the enumerator for TNode
  TNodeEnumerator = specialize TGNodeEnumerator<TNode>;

// Reverse Enumerator for nodes. Implemented as a generic so it may be reused
// For a node that implements TNode.
  generic TGNodeReverseEnumerator<T> = object
  protected
    // We store the parent node for the case if during iteration the child node
    // changes it's parent.
    FParent   : TNode;
    // Current node, returned by the iterator.
    FCurrent  : TNode;
    // Next node, held separately so we can go on smoothly event if the current
    // node changes it's parent.
    FNext     : TNode;
    // Returns current node as T. No checking is done!
    function GetCurrent: T;
  public
    // Creates the iterator
    constructor Create( Parent : TNode );
    // Moves the iterator and returns whether current if valid
    function MoveNext : Boolean;
    // Allows to be used as enumerator
    function GetEnumerator : TGNodeReverseEnumerator;
  public
    // Returns current node
    property Current : T read GetCurrent;
  end;

  // Specialization of the reverse enumerator for TNode
  TNodeReverseEnumerator = specialize TGNodeReverseEnumerator<TNode>;

// The base Valkyrie node class, implements data for object serialization and
// unique identification (see @link(TUIDStore)). One of the reasons for
// this serialization is to create a global Load/Save Mechanism.
// Implements a self-disposing tree-like structure. The base class of @link(TSystem),
// and considered a building block for the data structure of the program. At best,
// all the program nodes should be gathered in one tree -- that allows one-call
// disposal of all the allocated memory.

{ TNode }

TNode = class(TVObject, ILuaReferencedObject)
       // Standard constructor, zeroes all fields.
       constructor Create; virtual;
       // Lua constructor - registers the class with lua, and reads props
       constructor Create( const aID : AnsiString; aUID : Boolean );
       // Stream constructor, reads UID, and ID from stream, should be overriden.
       constructor CreateFromStream( Stream : TStream ); override;
       // Write Node to stream (UID and ID) should be overriden.
       procedure WriteToStream( Stream : TStream ); override;
       // zeroes all fields.
       procedure Clean; virtual;
       // Adds theChild as a child of current node.
       // The child is added to the END of the Child list.
       procedure   Add(theChild : TNode); virtual;
       // Executed when the node has changed it's parent (not in nil case). By
       // default does nothing.
       procedure ParentChanged; virtual;
       // Changes parent to Destination.
       // Error free.
       procedure   Move(Destination : TNode);
       // Basic recieveing method. Should be overriden.
       procedure   Receive(MSG : TMessage); virtual;
       // Removes self from Parent node, calls Parent.Remove(Self)
       // if parent present.
       procedure   Detach; virtual;
       // Removes child from this node, does nothing if child's parent
       // isn't self. Detach calls this, so this is always ran
       procedure   Remove( theChild : TNode ); virtual;
       // Destroys all children.
       procedure DestroyChildren;
       // Standard destructor, frees @link(UID), and destroys
       // children and siblings.
       destructor  Destroy; override;
       // Returns wether the node has a parent.
       function hasParent : boolean; override;
       // Returns wether the object has a child.
       function hasChild : boolean; override;
       // Returns wether the object is a TNode
       function isNode : boolean; override;
       // Returns wether the object is a first child
       function isFirstChild : boolean; 
       // Returns wether the object is the last child
       function isLastChild : boolean;
       // TNode Interface for @link(grdebug.Log).
       // Calls vdebug.Log, providing additional information on
       // the log-calling TNode.
       procedure   Log      (const aLogString       : Ansistring); override; overload;
       // TNode Interface for @link(grdebug.Log).
       // Calls vdebug.Log, providing additional information on
       // the log-calling TNode.
       procedure   Log      (Level : TLogLevel; const aLogString : Ansistring); override; overload;
       // Compare with other node (should be overriden). By default compares
       // by ID then by UID.
       function Compare( aOther : TNode ) : Boolean; virtual;
       // Find child by pointer
       function FindChild( Child : TNode; Recursive : Boolean = False ) : TNode; overload;
       // Find child by uid
       function FindChild( UID : TUID; Recursive : Boolean = False ) : TNode; overload;
       // Find child by id
       function FindChild( const ID : AnsiString; Recursive : Boolean = False ) : TNode; overload;
       // Enumerator support
       function GetEnumerator: TNodeEnumerator;
       // Reverse enumerator
       function Reverse : TNodeReverseEnumerator;
       // Lua interface - GetLuaIndex
       function GetLuaIndex   : Integer;
       // Lua interface - GetID
       function GetID         : AnsiString;
       // Lua interface - GetProtoTable
       function GetProtoTable : AnsiString;
       // Lua interface - GetProtoName
       function GetProtoName  : AnsiString;
       // Returns a property value from Lua __props of this instance
       function GetLuaProperty( const Index : AnsiString ) : Variant;
       // Sets a property value in Lua __props of this instance
       procedure SetLuaProperty( const Index : AnsiString; Value : Variant );
       // Returns a property value from Lua __props of this instance
       function GetLuaProperty( const aPath : array of const; aDefValue : Variant ) : Variant;
       // Sets a property value in Lua __props of this instance
       procedure SetLuaProperty( const aPath : array of const; aValue : Variant );
       // Returns if Lua hooks can be read from the object itself
       function HasVolatileHooks : Boolean; virtual;
       // Returns a value from the prototype table
       function GetLuaProtoValue( const Index : AnsiString ) : Variant;
       // Returns whether the object has the passed flag
       function GetFlag( aFlag : Byte ) : Boolean;
       // Returns whether the object has the passed flag
       procedure SetFlag( aFlag : Byte; aValue : Boolean );
       // Runs a script from the registered table by id and with self
       function RunHook( Hook : Word; const Args : array of Const ) : Variant;
       // Returns whether the object has the passed hook
       function HasHook( Hook : Word ) : Boolean;
       // Lua interface - Register API -- WARNING - registers TableName metatable!
       class procedure RegisterLuaAPI( const TableName : AnsiString );
       // Function for getting custom properties in Lua.
       // Should push given property to the passed state
       // Default implementation is no-op
       function GetProperty( L : PLua_State; const aPropertyName : AnsiString ) : Integer; virtual;
       // Function for setting custom properties in Lua.
       // Assume that the value to be set is in aValueIndex
       // Default implementation is no-op
       function SetProperty( L : PLua_State; const aPropertyName : AnsiString; aValueIndex : Integer ) : Boolean; virtual;
     protected
       // Lua interface - registers with LuaSystem
       procedure RegisterWithLua( aClassType : TClass = nil );
     protected
       // Unique IDentification number (@link(TUID))
       // Assigned by the @link(UIDs) singleton, unique.
       FUID          : TUID;
       // Identification Number of this Class - may be shared among similar
       // Classes.
       FID           : TIDN;
       // Lua Registry index.
       FLuaIndex     : LongInt;
       // Lua Class pointer for vluasystem
       FLuaClassInfo : Pointer;
       // Hooks
       FHooks        : TFlags;
       // Name
       FName         : AnsiString;
       // Flags
       FFlags        : TFlags;
     private
       // Link to the parent node.
       FParent     : TNode;
       // Link to first child node.
       FChild      : TNode;
       // Link to next node.
       FNext       : TNode;
       // Link to previous node.
       FPrev       : TNode;
       // Count of children nodes.
       FChildCount : DWord;
     public
       // Lua Properties
       property LuaProperties[ const Index : AnsiString ] : Variant read GetLuaProperty write SetLuaProperty;
       // Lua Properties
       property LuaProto[ const Index : AnsiString ] : Variant read GetLuaProtoValue;
       // Hooks property
       property Hooks[ Index : Word ] : Boolean read HasHook;
       // Flags property
       property Flags[ Index : Byte ] : Boolean read GetFlag write SetFlag;

     public
       property LuaClassInfo : Pointer read FLuaClassInfo;
       property UID        : TUID read FUID;
       property ID         : TIDN read FID;
       property Parent     : TNode read FParent;
       property Child      : TNode read FChild;
       property Next       : TNode read FNext;
       property Prev       : TNode read FPrev;
       property ChildCount : DWord read FChildCount;
     end;

  TNodeList = class;

// Enumerator for node lists. Implemented as a generic so it may be reused
// For a node list that stores TNode descendants.
// Note : Works only on packed lists!
  generic TGNodeListEnumerator<T> = object
  protected
    // Current node, returned by the iterator.
    FList     : TNodeList;
    // Next node, held separately so we can go on smoothly event if the current
    // node changes it's parent.
    FCurrent  : DWord;
    // Returns current node as T. No checking is done!
    function GetCurrent : T;
  public
    // Creates the iterator
    constructor Create( List : TNodeList );
    // Moves the iterator and returns whether current if valid
    function MoveNext : Boolean;
    // Returns current node
    property Current : T read GetCurrent;
  end;

  // Specialization of the enumerator for TNode
  TNodeListEnumerator = specialize TGNodeListEnumerator<TNode>;

// Reverse enumerator for node lists. Implemented as a generic so it may be reused
// For a node list that stores TNode descendants
// Note : Works only on packed lists!
  generic TGNodeListReverseEnumerator<T> = object
  protected
    // Current node, returned by the iterator.
    FList     : TNodeList;
    // Next node, held separately so we can go on smoothly event if the current
    // node changes it's parent.
    FCurrent  : DWord;
    // Returns current node as T. No checking is done!
    function GetCurrent : T;
  public
    // Creates the iterator
    constructor Create( List : TNodeList );
    // Moves the iterator and returns whether current if valid
    function MoveNext : Boolean;
    // Allows to be used as enumerator
    function GetEnumerator : TGNodeListReverseEnumerator;
  public
    // Returns current node
    property Current : T read GetCurrent;
  end;

  // Specialization of the enumerator for TNode
  TNodeListReverseEnumerator = specialize TGNodeListReverseEnumerator<TNode>;

 // A type to manage and pass ordered lists of Nodes, without
 // owning them. Out of conviniece, the Index is 1-based.
 type TNodeList = class( TVObject )
     // Constructs the list, MaxSize is the maximum amount of
     // items stored
     constructor Create( MaxSize : DWord );
     // Pushes an item into the first free slot of the list.
     // Returns false if there's no place to push, true otherwise.
     function Push( aItem : TNode ) : Boolean;
     // Finds an item on the list, if present returns it, if
     // not returns nil.
     function Find( aItem : TNode ) : TNode;
     // Finds by ID.
     function FindByID( const ID : AnsiString ) : TNode;
     // Finds by UID.
     function FindByUID( const UID : TUID ) : TNode;
     // Returns true if list has this item.
     function Has( aItem : TNode ) : Boolean;
     // Finds an item on the list, if present returns it, if
     // not returns nil.
     function Remove( aItem : TNode ) : Boolean;
     // Finds by ID.
     function Remove( const ID : AnsiString ) : Boolean;
     // Finds by UID.
     function Remove( const UID : TUID ) : Boolean;
     // Clears the list
     procedure Clear;
     // Sorts the list
     procedure Sort;
     // Moves items so, that there are no empty spaces inside.
     procedure Pack;
     // Returns last non-nil index (1-based), returns 0 if no
     // non-nil items present.
     // Note : Works properly only on Packed lists
     function LastIndex : DWord;
     // Destroys the list, but not the items stored on it
     destructor Destroy; override;
     // Enumerator support
     // Note : Works only on packed lists!
     function GetEnumerator : TNodeListEnumerator; inline;
     // Reverse enumerator support
     // Note : Works only on packed lists!
     function Reverse : TNodeListReverseEnumerator; inline;
   protected
     // Gets an item from the list. If index is out of range,
     // we return nil.
     function GetItem( aIndex : DWord ) : TNode;
     // Places an item on the list. If one is present in the
     // index, it is simply overwritten
     procedure SetItem( aIndex : DWord; aItem : TNode );
   private
     FItems   : array of TNode;
     FMaxSize : DWord;
     FSize    : DWord;
   public
     property Items[ aIndex : DWord ] : TNode read GetItem write SetItem; default;
     property Size : DWord     read FSize;
     property Capacity : DWord read FMaxSize;
   end;

// Generic override of TNodeList, to return typess that we want
generic TGNodeList<T> = class( TNodeList )
private
    public type TEnumeratorType        = specialize TGNodeListEnumerator<T>;
    public type TReverseEnumeratorType = specialize TGNodeListReverseEnumerator<T>;
    // Pushes an item into the first free slot of the list.
    // Returns false if there's no place to push, true otherwise.
    function Push( aItem : T ) : Boolean; reintroduce;
    // Finds an item on the list, if present returns it, if
    // not returns nil.
    function Find( aItem : T ) : T; reintroduce;
    // Finds by ID.
    function FindByID( const ID : AnsiString ) : T; reintroduce;
    // Finds by UID.
    function FindByUID( const UID : TUID ) : T; reintroduce;
    // Enumerator support
    // Note : Works only on packed lists!
    function GetEnumerator: TEnumeratorType; reintroduce;
    // Reverse enumerator support
    // Note : Works only on packed lists!
    function Reverse : TReverseEnumeratorType; reintroduce;
  protected
    // Gets an item from the list. If index is out of range,
    // we return nil.
    function GetItem( aIndex : DWord ) : T; inline;
    // Gets an item from the list. If index is out of range,
    // we return nil.
    procedure SetItem( aIndex : DWord; const aItem : T ); inline;
  public
    property Items[ aIndex : DWord ] : T read GetItem write SetItem; default;
end;

implementation
uses sysutils, typinfo, vluasystem, vluastate, vuid;

{ TGNodeEnumerator }

function TGNodeEnumerator.GetCurrent : T;
begin
  Exit( T(FCurrent) );
end;

constructor TGNodeEnumerator.Create ( Parent : TNode );
begin
  FParent  := Parent;
  FCurrent := nil;
  FNext    := Parent.Child;
end;

function TGNodeEnumerator.MoveNext : Boolean;
begin
  FCurrent := FNext;
  if FNext <> nil then FNext := FNext.Next;
  if FNext = FParent.Child then FNext := nil;
  Exit( FCurrent <> nil );
end;

{ TGNodeReverseEnumerator }

function TGNodeReverseEnumerator.GetCurrent : T;
begin
  Exit( T(FCurrent) );
end;

constructor TGNodeReverseEnumerator.Create ( Parent : TNode );
begin
  FParent  := Parent;
  FCurrent := nil;
  if Parent.Child = nil
    then FNext := nil
    else FNext := Parent.Child.Prev;
end;

function TGNodeReverseEnumerator.MoveNext : Boolean;
begin
  FCurrent := FNext;
  if FNext <> nil then
  begin
    FNext := FNext.Prev;
    if FNext = FParent.Child.Prev then FNext := nil;
  end;
  Exit( FCurrent <> nil );
end;

function TGNodeReverseEnumerator.GetEnumerator : TGNodeReverseEnumerator;
begin
  Exit( Self );
end;

{ TNode }

constructor TNode.Create;
begin
  Log(LOGDEBUG2,'Created.');
  Clean;
end;

constructor TNode.Create(const aID: AnsiString; aUID : Boolean );
var iCount : DWord;
begin
  Log(LOGDEBUG2,'Created.');
  Clean;
  FID           := aID;
  RegisterWithLua;
  if aUID and (UIDs <> nil) then FUID := UIDs.Register( Self );
  LuaSystem.State.SetPrototypeTable( Self, '__proto' );

  with LuaSystem.GetTable( [ GetProtoTable, ID ] ) do
  try
    with TLuaClassInfo( LuaClassInfo ) do
      for iCount in HookSet do
        if isFunction( Hooks[ iCount ] ) then
          Include( FHooks, iCount );
    FFlags := getFlags( 'flags' );
  finally
    Free;
  end;
end;

constructor TNode.CreateFromStream( Stream: TStream );
begin
  Log(LOGDEBUG2,'Created.');
  Clean;

  FID   := Stream.ReadAnsiString();
  FUID  := Stream.ReadQWord();
  FName := Stream.ReadAnsiString;

  Stream.Read( FFlags,    SizeOf(FFlags) );
  Stream.Read( FHooks,    SizeOf(FHooks) );

  if Stream.ReadByte > 0 then
  begin
    RegisterWithLua;
    if ( UIDs <> nil ) and ( FUID <> 0 ) then
      UIDs.Register( Self, FUID );

    LuaSystem.State.SetPrototypeTable( Self, '__proto' );
    LuaSystem.State.SubTableFromStream( Self ,'__props', Stream );

    if Stream.ReadByte = 1 then
      LuaSystem.State.NewSubTableFromStream( Self ,'__hooks', Stream );
  end;
end;

procedure TNode.WriteToStream( Stream: TStream );
begin
  Stream.WriteAnsiString( FID );
  Stream.WriteQWord( FUID );
  Stream.WriteAnsiString(FName);
  Stream.Write( FFlags, SizeOf(FFlags) );
  Stream.Write( FHooks, SizeOf(FHooks) );

  if FLuaIndex >= 0 then
  begin
    Stream.WriteByte(1);
    LuaSystem.State.SubTableToStream( Self ,'__props', Stream );
    if LuaSystem.State.HasSubTable( Self, '__hooks' ) then
    begin
      Stream.WriteByte(1);
      LuaSystem.State.SubTableToStream( Self ,'__hooks', Stream );
    end
    else
      Stream.WriteByte(0);
  end
  else
    Stream.WriteByte(0);
end;

procedure TNode.Clean;
begin
  FLuaIndex     := LUA_NOREF;
  FLuaClassInfo := nil;
  FChild        := nil;
  FParent       := nil;
  FHooks        := [];
  FFlags        := [];
  FNext         := Self;
  FPrev         := Self;
  FChildCount   := 0;
  FUID          := 0;
  FID           := '';
end;

procedure TNode.Add(theChild : TNode);
begin
  if theChild.FParent <> nil then theChild.Detach;
  theChild.FParent := Self;
  if FChild = nil then
    FChild := theChild
  else
  begin
    theChild.FPrev := FChild.FPrev;
    theChild.FNext := FChild;
    FChild.FPrev.FNext := theChild;
    FChild.FPrev      := theChild;
  end;  
  Inc(FChildCount);
  theChild.ParentChanged;
end;

procedure TNode.ParentChanged;
begin
  // noop
end;

function TNode.hasParent : boolean;
begin
  Exit(FParent <> nil);
end;

function TNode.hasChild : boolean;
begin
  Exit(FChild <> nil);
end;

function TNode.isNode : boolean; 
begin
  Exit(True);
end;

function TNode.isFirstChild : boolean; 
begin
  if FParent <> nil then
    Exit(FParent.FChild = Self)
  else Exit(False);
end;

function TNode.isLastChild : boolean; 
begin
  if FParent <> nil then
    if FParent.FChild <> nil then
      Exit(FParent.FChild.FNext = Self)
    else Exit(False)
  else Exit(False);
end;


procedure TNode.Detach;
begin
  if FParent <> nil then
    FParent.Remove( Self ) // this will call Detach again!
  else
  begin
    if (FPrev <> nil) then
        FPrev.FNext := FNext;
    if (FNext <> nil) then
        FNext.FPrev := FPrev;
    FPrev := Self;
    FNext := Self;
    FParent := nil;
  end;
end;

procedure TNode.Remove( theChild : TNode );
begin
  if theChild.FParent <> Self then Exit(); // signal error?
  if theChild = FChild then
  begin
    if theChild.FNext <> theChild then FChild := theChild.FNext
                                  else FChild := nil;
  end;
  theChild.FParent := nil;
  theChild.Detach;
  Dec(FChildCount);
end;

procedure TNode.DestroyChildren;
begin
  while FChild <> nil do
  begin
    FChild.Free;
  end;
end;


procedure TNode.Receive(MSG : TMessage);
begin
  case MSG.ID of
    0 :;
    //MSG_NODE_Destroy : begin FParent.Remove(Self); Self.Done; exit; end;
  else
//    Self.Log('Unknown message recieved (@1, ID: @2)',[Msg.ClassName,Msg.ID]);
  end;
  MSG.Free;
end;

procedure TNode.Move(Destination : TNode);
begin
  if FParent <> nil     then Detach;
  if Destination <> nil then Destination.Add(Self);
end;

destructor TNode.Destroy;
begin
  Detach;
  if (UIDs <> nil) and (FUID <> 0) then UIDs.Remove(FUID);
  while FChild <> nil do
  begin
    FChild.Free;
  end;
  if (LuaSystem <> nil) and (FLuaIndex <> LUA_NOREF) then
    LuaSystem.UnRegisterObject( Self );
  Log(LOGDEBUG2,'Destroyed.');
end;

procedure TNode.Log      (const aLogString       : Ansistring);
begin
  vdebug.Log('<'+classname+'/'+FID+'/'+IntToStr(FUID)+'> '+aLogString);
end;

procedure TNode.Log(Level: TLogLevel; const aLogString: Ansistring);
begin
  vdebug.Log(Level,'<'+classname+'/'+FID+'/'+IntToStr(FUID)+'> '+aLogString);
end;

function TNode.Compare ( aOther : TNode ) : Boolean;
begin
  if FID > aOther.FID then Exit( True );
  if UID > aOther.UID then Exit( False );
  Exit( False );
end;

function TNode.FindChild( Child: TNode; Recursive: Boolean ): TNode;
var Scan : TNode;
    Rec  : TNode;
begin
  Scan := FChild;
  if Scan <> nil then
  repeat
    if Scan = Child then Exit( Scan );
    Scan := Scan.Next;
  until Scan = FChild;
  if Recursive then
  begin
    Scan := FChild;
    if Scan <> nil then
    repeat
      Rec := Scan.FindChild( Child, Recursive );
      if Rec <> nil then Exit( Rec );
      Scan := Scan.Next;
    until Scan = FChild;
  end;
  Exit( nil );
end;

function TNode.FindChild( UID: TUID; Recursive: Boolean ): TNode;
var Scan : TNode;
    Rec  : TNode;
begin
  Scan := FChild;
  if Scan <> nil then
  repeat
    if Scan.UID = UID then Exit( Scan );
    Scan := Scan.Next;
  until Scan = FChild;
  if Recursive then
  begin
    Scan := FChild;
    if Scan <> nil then
    repeat
      Rec := Scan.FindChild( UID, Recursive );
      if Rec <> nil then Exit( Rec );
      Scan := Scan.Next;
    until Scan = FChild;
  end;
  Exit( nil );
end;

function TNode.FindChild( const ID: AnsiString; Recursive: Boolean ): TNode;
var Scan : TNode;
    Rec  : TNode;
begin
  Scan := FChild;
  if Scan <> nil then
  repeat
    if Scan.ID = ID then Exit( Scan );
    Scan := Scan.Next;
  until Scan = FChild;
  if Recursive then
  begin
    Scan := FChild;
    if Scan <> nil then
    repeat
      Rec := Scan.FindChild( ID, Recursive );
      if Rec <> nil then Exit( Rec );
      Scan := Scan.Next;
    until Scan = FChild;
  end;
  Exit( nil );
end;

function TNode.GetEnumerator : TNodeEnumerator;
begin
  GetEnumerator.Create(Self);
end;

function TNode.Reverse : TNodeReverseEnumerator;
begin
  Reverse.Create(Self);
end;

function TNode.GetLuaIndex: Integer;
begin
  Exit( FLuaIndex );
end;

function TNode.GetID: AnsiString;
begin
  Exit( FID );
end;

function TNode.GetProtoTable: AnsiString;
begin
  Exit( TLuaClassInfo(FLuaClassInfo).Storage );
end;

function TNode.GetProtoName: AnsiString;
begin
  Exit( TLuaClassInfo(FLuaClassInfo).Proto );
end;

procedure TNode.RegisterWithLua( aClassType : TClass = nil );
begin
  if FLuaIndex <> LUA_NOREF then raise EException.Create('Register With Lua called twice!');
  if aClassType = nil then aClassType := Self.ClassType;
  FLuaIndex := LuaSystem.RegisterObject( Self, aClassType.ClassName );
  FLuaClassInfo := LuaSystem.GetClassInfo( aClassType );
end;

function TNode.GetProperty ( L : PLua_State; const aPropertyName : AnsiString ) : Integer;
begin
  Exit( 0 );
end;

function TNode.SetProperty ( L : PLua_State; const aPropertyName : AnsiString; aValueIndex : Integer ) : Boolean;
begin
  Exit( False );
end;

function TNode.GetLuaProperty ( const Index : AnsiString ) : Variant;
begin
  Exit( LuaSystem.State.GetLuaProperty( Self, Index ) );
end;

function TNode.HasVolatileHooks: Boolean;
begin
  Exit( False );
end;

procedure TNode.SetLuaProperty ( const Index : AnsiString; Value : Variant ) ;
begin
  LuaSystem.State.SetLuaProperty( Self, Index, Value );
end;

function TNode.GetLuaProperty(const aPath: array of const; aDefValue: Variant ): Variant;
begin
  Exit( LuaSystem.State.GetLuaProperty( Self, aPath, aDefValue ) );
end;

procedure TNode.SetLuaProperty(const aPath: array of const; aValue: Variant);
begin
  LuaSystem.State.SetLuaProperty( Self, aPath, aValue );
end;

function TNode.GetLuaProtoValue ( const Index : AnsiString ) : Variant;
begin
  Exit( LuaSystem.Get( [ GetProtoTable, ID, Index ] ) );
end;

function TNode.GetFlag ( aFlag : Byte ) : Boolean;
begin
  Exit( aFlag in FFlags );
end;

procedure TNode.SetFlag ( aFlag : Byte; aValue : Boolean ) ;
begin
  if aValue
    then Include( FFlags, aFlag )
    else Exclude( FFlags, aFlag );
end;

function TNode.RunHook ( Hook : Word; const Args : array of const ) : Variant;
begin
  RunHook := False;
  if Hook in FHooks then
    RunHook := LuaSystem.ProtectedRunHook( Self, TLuaClassInfo( LuaClassInfo ).Hooks[ Hook ], Args );
end;

function TNode.HasHook ( Hook : Word ) : Boolean;
begin
  Exit( Hook in FHooks );
end;


procedure TVObject.Log      (const aLogString       : Ansistring);
begin
  vdebug.Log('<'+classname+'> '+aLogString);
end;

procedure TVObject.Log( aLevel: TLogLevel; const aLogString: Ansistring );
begin
  vdebug.Log( aLevel,'<'+classname+'> '+aLogString );
end;

procedure TVObject.Log( const aLogString: Ansistring; const aParam: array of const );
begin
  Log( Format( aLogString, aParam ) );
end;

procedure TVObject.Log( aLevel: TLogLevel; const aLogString: Ansistring; const aParam: array of const);
begin
  Log( aLevel, Format( aLogString, aParam ) );
end;

function TVObject.hasParent : boolean;
begin
  Exit(False);
end;

function TVObject.hasChild : boolean;
begin
  Exit(False);
end;

function TVObject.isNode : boolean; 
begin
  Exit(False);
end;

constructor TVObject.CreateFromStream ( Stream : TStream ) ;
begin
  // noop
end;

procedure TVObject.WriteToStream ( Stream : TStream ) ;
begin
  // noop
end;

{ TGNodeListEnumerator }

function TGNodeListEnumerator.GetCurrent : T;
begin
  Exit( T(FList[ FCurrent ]) );
end;

constructor TGNodeListEnumerator.Create ( List : TNodeList ) ;
begin
  FList    := List;
  FCurrent := 0;
end;

function TGNodeListEnumerator.MoveNext : Boolean;
begin
  repeat
    Inc( FCurrent );
    if FCurrent > FList.Size then Exit( False );
  until FList[ FCurrent ] <> nil;
  Exit( True );
end;

{ TGNodeListEnumerator }

function TGNodeListReverseEnumerator.GetCurrent : T;
begin
  Exit( T(FList[ FCurrent ]) );
end;

constructor TGNodeListReverseEnumerator.Create ( List : TNodeList ) ;
begin
  FList    := List;
  FCurrent := List.LastIndex+1;
end;

function TGNodeListReverseEnumerator.MoveNext : Boolean;
begin
  repeat
    Dec( FCurrent );
    if FCurrent = 0 then Exit( False );
  until FList[ FCurrent ] <> nil;
  Exit( True );
end;

function TGNodeListReverseEnumerator.GetEnumerator : TGNodeListReverseEnumerator;
begin
  Exit( Self );
end;

{ TNodeList }

constructor TNodeList.Create ( MaxSize : DWord ) ;
begin
  inherited Create;
  FSize    := 0;
  FMaxSize := MaxSize;
  SetLength( FItems, MaxSize );
  FillChar( FItems[0], MaxSize*SizeOf( TNode ), 0 );
end;

function TNodeList.Push ( aItem : TNode ) : Boolean;
var aIndex : DWord;
begin
  aIndex := 0;
  while FItems[ aIndex ] <> nil do
    if aIndex = FMaxSize-1
      then Exit( False )
      else Inc( aIndex );
  FItems[ aIndex ] := aItem;
  Inc( FSize );
  Exit( True );
end;

function TNodeList.GetItem ( aIndex : DWord ) : TNode;
begin
  if (aIndex = 0) or (aIndex > FMaxSize) then Exit( nil );
  Exit( FItems[ aIndex - 1 ] );
end;

procedure TNodeList.SetItem ( aIndex : DWord; aItem : TNode ) ;
begin
  if (aIndex = 0) or (aIndex > FMaxSize) then raise EException.Create('Bad index passed to SetItem!');
  Dec( aIndex );
  if FItems[ aIndex ] <> nil then Dec( FSize );
  if aItem <> nil            then Inc( FSize );
  FItems[ aIndex ] := aItem;
end;

function TNodeList.Find ( aItem : TNode ) : TNode;
var iCount : DWord;
begin
  for iCount := 0 to FMaxSize - 1 do
    if FItems[ iCount ] = aItem then
      Exit( aItem );
  Exit( nil );
end;

function TNodeList.FindByID ( const ID : AnsiString ) : TNode;
var iCount : DWord;
begin
  for iCount := 0 to FMaxSize - 1 do
    if ( FItems[ iCount ] <> nil ) and ( FItems[ iCount ].ID = ID ) then
      Exit( FItems[ iCount ] );
  Exit( nil );
end;

function TNodeList.FindByUID ( const UID : TUID ) : TNode;
var iCount : DWord;
begin
  for iCount := 0 to FMaxSize - 1 do
    if ( FItems[ iCount ] <> nil ) and ( FItems[ iCount ].UID = UID ) then
      Exit( FItems[ iCount ] );
  Exit( nil );
end;

function TNodeList.Has ( aItem : TNode ) : Boolean;
begin
  Exit( Find( aItem ) <> nil );
end;

function TNodeList.Remove ( aItem : TNode ) : Boolean;
var iCount : DWord;
begin
  for iCount := 0 to FMaxSize - 1 do
    if FItems[ iCount ] = aItem then
    begin
      FreeAndNil( FItems[ iCount ] );
      Dec( FSize );
      Exit( True );
    end;
  Exit( False );
end;

function TNodeList.Remove ( const ID : AnsiString ) : Boolean;
var iCount : DWord;
begin
  for iCount := 0 to FMaxSize - 1 do
    if ( FItems[ iCount ] <> nil ) and ( FItems[ iCount ].ID = ID ) then
    begin
      FreeAndNil( FItems[ iCount ] );
      Dec( FSize );
      Exit( True );
    end;
  Exit( False );
end;

function TNodeList.Remove ( const UID : TUID ) : Boolean;
var iCount : DWord;
begin
  for iCount := 0 to FMaxSize - 1 do
    if ( FItems[ iCount ] <> nil ) and ( FItems[ iCount ].UID = UID ) then
    begin
      FreeAndNil( FItems[ iCount ] );
      Dec( FSize );
      Exit( True );
    end;
  Exit( False );
end;

procedure TNodeList.Clear;
var iCount : DWord;
begin
  for iCount := 0 to FMaxSize-1 do
    FItems[ iCount ] := nil;
end;

procedure TNodeList.Sort;
var cn   : DWord;
    ci   : DWord;
  procedure Swap( var Item1, Item2 : TNode );
  var Temp : TNode;
  begin
    Temp  := Item1;
    Item1 := Item2;
    Item2 := Temp;
  end;
begin
  Pack;
  if FSize > 1 then
  for cn := 0 to FSize-2 do
    for ci := 0 to FSize-2-cn do
      if FItems[ci].Compare( FItems[ci+1] ) then
        Swap(FItems[ci],FItems[ci+1]);
end;

procedure TNodeList.Pack;
var iCount : DWord;
    iPos   : DWord;
    iFound : Boolean;
begin
  for iCount := 0 to FMaxSize-2 do
    if FItems[iCount] = nil then
    begin
      iFound := False;
      for iPos := iCount+1 to FMaxSize-1 do
        if FItems[iPos] <> nil then
        begin
          FItems[iCount] := FItems[iPos];
          FItems[iPos]   := nil;
          iFound         := True;
          Break;
        end;
      if not iFound then Break;
    end;
end;

function TNodeList.LastIndex : DWord;
var iCount : DWord;
begin
  for iCount := FMaxSize-1 downto 0 do
    if FItems[iCount] <> nil then
      Exit( iCount + 1 );
  Exit( 0 );
end;

destructor TNodeList.Destroy;
begin
  SetLength( FItems, 0 );
  inherited Destroy;
end;

function TNodeList.GetEnumerator : TNodeListEnumerator;
begin
  GetEnumerator.Create( Self );
end;

function TNodeList.Reverse : TNodeListReverseEnumerator;
begin
  Reverse.Create( Self );
end;

{ TGNodeList }

procedure TGNodeList.SetItem ( aIndex : DWord ; const aItem : T ) ;
begin
  inherited SetItem( aIndex, aItem );
end;

function TGNodeList.Push ( aItem : T ) : Boolean;
begin
  Exit( inherited Push( aItem ) );
end;

function TGNodeList.GetItem ( aIndex : DWord ) : T;
begin
  Exit( T(inherited GetItem( aIndex )) );
end;

function TGNodeList.Find ( aItem : T ) : T;
begin
  Exit( T(inherited Find( aItem )) );
end;

function TGNodeList.FindByID ( const ID : AnsiString ) : T;
begin
  Exit( T(inherited FindByID( ID )) );
end;

function TGNodeList.FindByUID ( const UID : TUID ) : T;
begin
  Exit( T(inherited FindByUID( UID )) );
end;

function TGNodeList.GetEnumerator : TEnumeratorType;
begin
  GetEnumerator.Create( Self );
end;

function TGNodeList.Reverse : TReverseEnumeratorType;
begin
  Reverse.Create( Self );
end;

function lua_node_get_type(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  State.Push( LuaSystem.GetProtoTable( Node.ClassType ) );
  Result := 1;
end;

function lua_node_get_id(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  State.Push( Node.ID );
  Result := 1;
end;

function lua_node_get_uid(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  State.Push( LongInt(Node.UID) );
  Result := 1;
end;

function lua_node_get_parent(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  State.Push( Node.Parent as ILuaReferencedObject );
  Result := 1;
end;

function lua_node_get_child_count(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  State.Push( LongInt(Node.ChildCount) );
  Result := 1;
end;

function lua_node_children_closure(L: Plua_State): Integer; cdecl;
var State     : TLuaState;
    Parent    : TNode;
    Next      : TNode;
    Current   : TNode;
begin
  State.Init( L );
  Parent    := TObject( lua_touserdata( L, lua_upvalueindex(1) ) ) as TNode;
  Next      := TObject( lua_touserdata( L, lua_upvalueindex(2) ) ) as TNode;

  Current := Next;
  if Next <> nil then Next := Next.Next;
  if Next = Parent.Child then Next := nil;
  lua_pushlightuserdata( L, Next );
  lua_replace( L, lua_upvalueindex(2) );

  State.Push( Current );
  Exit( 1 );
end;

function lua_node_children_filter_closure(L: Plua_State): Integer; cdecl;
var State     : TLuaState;
    Parent    : TNode;
    Next      : TNode;
    Current   : TNode;
    Filter    : AnsiString;
begin
  State.Init( L );
  Parent    := TObject( lua_touserdata( L, lua_upvalueindex(1) ) ) as TNode;
  Next      := TObject( lua_touserdata( L, lua_upvalueindex(2) ) ) as TNode;
  Filter    := lua_tostring( L, lua_upvalueindex(3) );

  repeat
    Current := Next;
    if Next <> nil then Next := Next.Next;
    if Next = Parent.Child then Next := nil;
  until (Current = nil) or (Current.GetProtoName = Filter);

  lua_pushlightuserdata( L, Next );
  lua_replace( L, lua_upvalueindex(2) );

  State.Push( Current );
  Exit( 1 );
end;

// iterator
function lua_node_children(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  lua_pushlightuserdata( L, Node );
  lua_pushlightuserdata( L, Node.Child );
  if lua_isstring( L, 2 ) then
  begin
    lua_pushvalue( L, 2 );
    lua_pushcclosure( L, @lua_node_children_filter_closure, 3 );
  end
  else
    lua_pushcclosure( L, @lua_node_children_closure, 2 );
  Exit( 1 );
end;

function lua_node_property_set(L: Plua_State): Integer; cdecl;
var State  : TLuaState;
    Node   : TNode;
    Prop   : AnsiString;
    HookID : Integer;
begin
  State.Init(L);
  lua_settop( L, 3 );

  // check __props
  lua_getfield( L, 1, '__props' );
  lua_pushvalue( L, 2 ); // key
  lua_rawget( L, -2 );
  if not lua_isnil( L, -1 ) then
  begin
    lua_settop( L, 4 ); // leave __props
    lua_pushvalue( L, 2 ); // key
    lua_pushvalue( L, 3 ); // value
    lua_rawset( L, -3 );
    Exit(0);
  end;

  lua_settop( L, 3 );
  // check game object -- TODO - unwrap ToObject and ToString for efficiency
  Node := State.ToObject(1) as TNode;
  Prop := State.ToString(2);
  if Node.SetProperty( L, Prop, 3 ) then Exit(0);
  if GetPropInfo( Node.ClassType, Prop ) <> nil then
  begin
    SetPropValue( Node as Node.ClassType, Prop, State.ToVariant(3) );
    Exit( 0 );
  end;
  if Node.HasVolatileHooks and (Node.LuaClassInfo <> nil) then
  begin
    HookID := TLuaClassInfo( Node.LuaClassInfo ).GetHookID( Prop );
    if (HookID >= 0) and (lua_isnil(L,3) or lua_isfunction(L,3)) then
    begin
      if lua_isnil(L,3)
        then Exclude( Node.FHooks, HookID )
        else Include( Node.FHooks, HookID );
      lua_rawset( L, -3 );
      Exit(0);
    end;
  end;
  State.Error('Unknown property "'+Prop+'" requested on object of type '+Node.ClassName+'!');
  Result := 0;
end;

function lua_node_property_get(L: Plua_State): Integer; cdecl;
var State   : TLuaState;
    Node    : TNode;
    Prop    : AnsiString;
    Res     : Variant;
    PInfo   : ^TPropInfo;
begin
  State.Init(L);
  lua_settop( L, 2 );

  // check __props
  lua_getfield( L, 1, '__props' );
  lua_pushvalue( L, 2 ); // key
  lua_rawget( L, -2 );
  if not lua_isnil( L, -1 ) then Exit(1);
  lua_settop( L, 2 );

  // check game object TODO - unwrap ToObject and ToString for efficiency
  Node := State.ToObject(1) as TNode;
  Prop := State.ToString(2);
  Result := Node.GetProperty( L, Prop );
  if Result > 0 then Exit;
  PInfo := GetPropInfo(Node, Prop);
  if PInfo = nil then
    State.Error('Unknown property "'+Prop+'" requested on object of type '+Node.ClassName+'!');
  Res := GetPropValue(Node as Node.ClassType, Prop, False );
  if PInfo^.PropType^.Kind = tkBool then VarCast( Res, Res, varBoolean );
  State.PushVariant( Res );
  Result := 1;
end;

function lua_node_property_add(L: Plua_State): Integer; cdecl;
begin
  lua_settop( L, 3 );
  lua_getfield( L, 1, '__props' );
  lua_pushvalue( L, 2 ); // key
  if lua_isnoneornil( L, 3 ) then
    lua_pushboolean( L, true )
  else
    lua_pushvalue( L, 3 );
  lua_rawset( L, -3 );
  Result := 0;
end;

function lua_node_property_remove(L: Plua_State): Integer; cdecl;
begin
  lua_settop( L, 3 );
  lua_getfield( L, 1, '__props' );
  lua_pushvalue( L, 2 ); // key
  lua_pushnil( L );
  lua_rawset( L, -3 );
  Result := 0;
end;

function lua_node_property_has(L: Plua_State): Integer; cdecl;
begin
  lua_settop( L, 2 );
  lua_getfield( L, 1, '__props' );
  lua_pushvalue( L, 2 ); // key
  lua_rawget( L, -2 );
  lua_pushboolean( L, not lua_isnil( L, -1 ) );
  Result := 1;
end;

function lua_node_add(L: Plua_State): Integer; cdecl;
var State : TLuaState;
    Node  : TNode;
begin
  State.Init(L);
  Node := State.ToObject(1) as TNode;
  Node.Add( State.ToObject(2) as TNode);
  Result := 0;
end;

function lua_node_register_hook(L: Plua_State): Integer; cdecl;
var State  : TLuaState;
    GNode  : TNode;
    Hook   : AnsiString;
    ID     : Integer;
begin
  State.Init(L);
  lua_settop( L, 3 );
  GNode := State.ToObject(1) as TNode;
  Hook  := State.ToString(2);
  ID := TLuaClassInfo(GNode.LuaClassInfo).GetHookID(Hook);
  if ID = -1 then
    State.Error('Unknown hook "'+Hook+'" requested to be set on object of type '+GNode.ClassName+'!');

  if not lua_isnil( L, 3 ) then
  begin
    // Check if hook exists
    lua_getglobal( L, GNode.GetProtoTable );
    lua_pushansistring( L, '__hooks' );
    lua_rawget( L, -2 );
    if lua_isnil( L, -1 ) then
      State.Error('Table "'+GNode.GetProtoTable+'" has no __hooks registry!');
    lua_pushvalue( L, 3 );
    lua_rawget( L, -2 );
    if lua_isnil( L, -1 ) then
      State.Error('Hook "'+Hook+'" requested to be set with an unknown __hooks value!');
    lua_settop( L, 3 );
  end;

  // Set it
  lua_pushansistring( L, '__hooks' );
  lua_rawget( L, 1 );
  if lua_isnil( L, -1 ) then
  begin
    if lua_isnil( L, 3 ) then Exit( 0 );
    lua_settop( L, 3 );
    lua_pushansistring( L, '__hooks' );
    lua_createtable( L, 0, 0 );
    lua_rawset( L, 1 );
    lua_pushansistring( L, '__hooks' );
    lua_rawget( L, 1 );
  end;
  lua_pushansistring( L, Hook );
  lua_pushvalue( L, 3 );
  lua_rawset( L, -3 );
  if lua_isnil( L, 3 ) then
  begin
    if not LuaSystem.Defined([ GNode.GetProtoTable, GNode.ID, Hook ]) then
      Exclude( GNode.FHooks, ID );
  end
  else
    Include( GNode.FHooks, ID );
  Exit( 0 );
end;

function lua_node_flags_get(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iNode  : TNode;
begin
  iState.Init( L );
  if iState.StackSize < 2 then Exit(0);
  iNode := iState.ToObject(1) as TNode;
  iState.Push( iState.ToInteger(2) in iNode.FFlags);
  Result := 1;
end;

function lua_node_flags_set(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iNode  : TNode;
    iFlag  : byte;
begin
  iState.Init( L );
  if iState.StackSize < 3 then Exit(0);
  iNode := iState.ToObject(1) as TNode;
  iFlag := iState.ToInteger(2);
  if iState.ToBoolean(3)
    then Include( iNode.FFlags, iFlag )
    else Exclude( iNode.FFlags, iFlag );
  Result := 0;
end;

function lua_node_destroy(L: Plua_State): Integer; cdecl;
var iState : TLuaState;
    iNode  : TNode;
begin
  iState.Init(L);
  iNode := iState.ToObject(1) as TNode;
  iNode.Free;
  Result := 0;
end;

const lua_node_lib : array[0..14] of luaL_Reg = (
      ( name : 'add';             func : @lua_node_add),
      ( name : 'get_type';        func : @lua_node_get_type),
      ( name : 'get_id';          func : @lua_node_get_id),
      ( name : 'get_uid';         func : @lua_node_get_uid),
      ( name : 'get_parent';      func : @lua_node_get_parent),
      ( name : 'get_child_count'; func : @lua_node_get_child_count),
      ( name : 'children';        func : @lua_node_children),
      ( name : 'get_property';    func : @lua_node_property_get),
      ( name : 'set_property';    func : @lua_node_property_set),
      ( name : 'add_property';    func : @lua_node_property_add),
      ( name : 'has_property';    func : @lua_node_property_has),
      ( name : 'remove_property'; func : @lua_node_property_remove),
      ( name : 'register_hook';   func : @lua_node_register_hook),
      ( name : 'destroy';         func : @lua_node_destroy),
      ( name : nil;               func : nil; )
);

class procedure TNode.RegisterLuaAPI(const TableName: AnsiString);
begin
  LuaSystem.Register( TableName, lua_node_lib );
  LuaSystem.RegisterSubTable( TableName, '__props' );
  LuaSystem.RegisterMetaTable( TableName, 'flags', @lua_node_flags_get, @lua_node_flags_set );
  LuaSystem.RegisterMetaTable( TableName, @lua_node_property_get, @lua_node_property_set );
end;



end.

// Modified      : $Date: 2008-01-14 22:16:41 +0100 (Mon, 14 Jan 2008) $
// Last revision : $Revision: 110 $
// Last author   : $Author: chaos-dev $
// Last commit   : $Log$
// Head URL      : $HeadURL: https://libvalkyrie.svn.sourceforge.net/svnroot/libvalkyrie/fp/src/vnode.pas $

