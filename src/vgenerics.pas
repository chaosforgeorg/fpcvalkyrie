{$INCLUDE valkyrie.inc}
// @abstract(Generic data structures for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @cvs($Author: chaos-dev $)
//
// TODO: TGArray<AnsiString>.IndexOf wont work, because of raw comparison.
//       Non-raw comparison can be done, but them objects don't work (no
//       operator detection. Either do a dedicated TStringArray or separate
//       TGBuiltInArray.
unit vgenerics;
interface
uses types, classes, sysutils, vnode, vstream;

type ERangeError       = class(Exception);
     ECollisionError   = class(Exception);
     EUndefinedError   = class(Exception);

     TRawPointerCompareFunc = function( aIndex1, aIndex2 : Pointer): Integer of object;

type TRawPointerArray = class(TVObject)
  protected
    FData     : PByte;
    FCount    : Integer;
    FCapacity : Integer;
    FItemSize : Integer;
  protected
    procedure CopyItem( aFrom, aTo : Pointer ); virtual;
    procedure DisposeOf( aItem : Pointer ); virtual;
    procedure DisposeOfRange( aFromIndex, aToIndex: Integer );
    procedure Expand;
    procedure ExpandTo( aCapacity : Integer );
    function InternalPush( aItem : Pointer ) : Integer;
    function InternalPop : Integer;
    function InternalTop : Pointer;
    procedure InternalPut( aIndex : Integer; aItem : Pointer );
    function InternalGet( aIndex : Integer ) : Pointer;
    procedure InternalSwap( aIndex1, aIndex2 : Integer );
    function InternalInsert( aIndex : Integer ) : Pointer;
    procedure InternalInsert( aIndex : Integer; aItem : Pointer );
    procedure SetCapacity( aCapacity : Integer );
    procedure SetCount( aCount : Integer );
    procedure QuickSort( aL, aR : Integer; aCompare : TRawPointerCompareFunc );
  public
    constructor Create( aItemSize : Integer = sizeof(Pointer) );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Reset; virtual;
    procedure PointerSort( aCompare : TRawPointerCompareFunc );
    procedure Delete( aIndex : Integer ); virtual;
    procedure Reserve( aCapacity : Integer );
    procedure Resize( aSize : Integer );
    function IsEmpty : Boolean;

    property Items[Index: Integer] : Pointer read InternalGet write InternalPut; default;
    property ItemSize : Integer read FItemSize;
    property Size : Integer read FCount;
    property Capacity : Integer read FCapacity;
    property Data : PByte read FData;
  end;

type
  generic TGArrayEnumerator<T> = object
  protected
    FArray    : TRawPointerArray;
    FPosition : Integer;
    function GetCurrent: T;
  public
    constructor Create( aArray : TRawPointerArray );
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  generic TGArray<T> = class(TRawPointerArray)
  public type
    TTypeArrayEnumerator = specialize TGArrayEnumerator<T>;
    TCompareFunc         = function ( const aItem1, aItem2 : T ): Integer;
    TTypeArray = array[0..(MaxInt div 1024)] of T;
    PTypeArray = ^TTypeArray;
    TType      = T;
    PType      = ^T;
  var protected
    FOnCompare : TCompareFunc;
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
    function GetData : PTypeArray;
    function PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
  public
    constructor Create;
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    procedure Sort( aCompare : TCompareFunc );
    function Push( const aItem : T ) : Integer;
    function Pop : T;
    function Top : T;
    procedure Put( aIndex : Integer; const aItem : T );
    function Get( aIndex : Integer ) : T;
    function IndexOf( const aItem : T ) : Integer;
    function GetEnumerator: TTypeArrayEnumerator;

    property Items[ Index: Integer ] : T read Get write Put; default;
    property Data : PTypeArray read GetData;
    property Size : Integer read FCount write SetCount;
    property Capacity : Integer read FCapacity write SetCapacity;
  end;

  generic TGObjectArray<T> = class(TRawPointerArray)
  public type
    TTypeArrayEnumerator = specialize TGArrayEnumerator<T>;
    TCompareFunc         = function ( const aItem1, aItem2 : T ): Integer;
    TTypeArray = array[0..(MaxInt div 1024)] of T;
    PTypeArray = ^TTypeArray;
    TType      = T;
    PType      = ^T;
  var protected
    FOnCompare : TCompareFunc;
    FManaged   : Boolean;
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
    function GetData : PTypeArray;
    function PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
  public
    constructor Create( aManaged : Boolean = True );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    procedure Sort( aCompare : TCompareFunc );
    function Push( const aItem : T ) : Integer;
    function Pop : T;
    function Top : T;
    procedure Put( aIndex : Integer; const aItem : T );
    function Get( aIndex : Integer ) : T;
    function IndexOf( const aItem : T ) : Integer;
    function GetEnumerator: TTypeArrayEnumerator;

    property Items[ Index: Integer ] : T read Get write Put; default;
    property Data : PTypeArray read GetData;
    property Size : Integer read FCount write SetCount;
    property Capacity : Integer read FCapacity write SetCapacity;
  end;

  TRawHashMap = class;
  TRawHashMapBucket = class( TObject )
  private
    FValues   : PByte;
    FKeys     : array of AnsiString;
    FItemSize : Integer;
    FCapacity : Integer;
    FCount    : Integer;
    FHashMap  : TRawHashMap;
  public
    constructor Create( aHashMap : TRawHashMap );
    procedure Add( const aKey : AnsiString; aValue : Pointer );
    function FindIndex( const aKey : AnsiString ) : Integer;
    function GetValue( const aKey : AnsiString ) : Pointer;
    function GetValue( aIndex : Integer ) : Pointer;
    procedure SetValue( aIndex : Integer; aValue : Pointer );
    function Remove( const aKey : AnsiString ) : Boolean;
    function GetKey( aIndex : Integer ) : AnsiString;
    destructor Destroy; override;

    property Count : Integer read FCount;
  end;

  THashMapPolicy = ( HashMap_NoRaise, HashMap_RaiseCollision, HashMap_RaiseUndefined, HashMap_RaiseAll );

  TRawHashMap = class( TVObject )
  protected
    FPolicy    : THashMapPolicy;
    FBucket    : array of TRawHashMapBucket;
    FBuckets   : Integer;
    FCount     : Integer;
    FItemSize  : Integer;
    FLastQuery : AnsiString;
    FLastValue : Pointer;
  protected
    procedure CopyItem( aFrom, aTo : Pointer ); virtual;
    procedure DisposeOf( aItem : Pointer ); virtual;
    function Query( const aKey : AnsiString ) : Pointer;
    function Hash( const aKey : AnsiString ) : Integer;
    function InternalAdd( const aKey : AnsiString; aValue : Pointer ) : Boolean;
  public
    function LinearGet( aBIdx, aIIdx : Integer ) : Pointer;
    function LinearGetKey( aBIdx, aIIdx : Integer ) : AnsiString;
    function BucketSize( aBIdx : Integer ) : Integer;
    constructor Create( aPolicy : THashMapPolicy = HashMap_NoRaise; aBuckets : Integer = 94; aItemSize : Integer = sizeof(Pointer) );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    procedure Clear;
    destructor Destroy; override;
    function Exists( const aKey : AnsiString ) : Boolean;
    function Remove( const aKey : AnsiString ) : Boolean;

    property ItemSize : Integer read FItemSize;
    property Buckets : Integer read FBuckets;
    property Size : Integer read FCount;
  end;

type generic TPair<TKey,TValue> = object Key : TKey; Value : TValue; end;

type
  generic TGHashMapEnumerator<T> = class(TObject)
  public type
    TPairType = specialize TPair<AnsiString,T>;
  var protected
    FHashMap     : TRawHashMap;
    FBIdx, FIIdx : Integer;
    FISize       : Integer;
    FCurrent     : Pointer;
    FKey         : AnsiString;
    function GetCurrent: TPairType;
  public
    constructor Create( aHashMap : TRawHashMap );
    function MoveNext: Boolean;
    property Current: TPairType read GetCurrent;
  end;

  generic TGHashMap<T> = class(TRawHashMap)
  public type
    TTypeHashMapEnumerator = specialize TGHashMapEnumerator<T>;
    TPairType              = specialize TPair<AnsiString,T>;
  var protected
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
  public
    constructor Create( aPolicy : THashMapPolicy = HashMap_NoRaise; aBuckets : Integer = 94 );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    procedure Put( const aKey : AnsiString; const aValue : T );
    function Get( const aKey : AnsiString ) : T;
    function Get( const aKey : AnsiString; const DefVal : T ) : T;

    function GetEnumerator: TTypeHashMapEnumerator;
    property Items[ const aKey : Ansistring ] : T read Get write Put; default;
  end;

  generic TGObjectHashMap<T> = class(TRawHashMap)
  public type
    TTypeHashMapEnumerator = specialize TGHashMapEnumerator<T>;
    TPairType              = specialize TPair<AnsiString,T>;
  var protected
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
  protected
    FManaged : Boolean;
  public
    constructor Create( aManaged : Boolean = True; aPolicy : THashMapPolicy = HashMap_NoRaise; aBuckets : Integer = 94 );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    procedure Put( const aKey : AnsiString; const aValue : T );
    function Get( const aKey : AnsiString ) : T;

    function GetEnumerator: TTypeHashMapEnumerator;
    property Items[ const aKey : Ansistring ] : T read Get write Put; default;
  end;

  type TRawRingBuffer = class(TVObject)
  protected
    FData     : PByte;
    FCount    : Integer;
    FCapacity : Integer;
    FItemSize : Integer;
    FStart    : Integer;
  protected
    procedure CopyItem( aFrom, aTo : Pointer ); virtual;
    procedure DisposeOf( aItem : Pointer ); virtual;
    procedure DisposeOfRange( aFromIndex, aToIndex: Integer );
    function InternalPushFront( aItem : Pointer ) : Integer;
    function InternalPushBack( aItem : Pointer ) : Integer;
    function InternalPopFront : Integer;
    function InternalPopBack : Integer;
    function InternalFront : Pointer;
    function InternalBack : Pointer;
    procedure InternalPut( aIndex : Integer; aItem : Pointer );
    function InternalGet( aIndex : Integer ) : Pointer;
  public
    constructor Create( aCapacity : Integer; aItemSize : Integer = sizeof(Pointer) );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    destructor Destroy; override;
    procedure Clear; virtual;

    property Items[Index: Integer] : Pointer read InternalGet write InternalPut; default;
    property ItemSize : Integer read FItemSize;
    property Size     : Integer read FCount;
    property Capacity : Integer read FCapacity;
  end;

  generic TGRingBufferEnumerator<T> = object
  protected
    FBuffer   : TRawRingBuffer;
    FPosition : Integer;
    function GetCurrent: T;
  public
    constructor Create( aBuffer : TRawRingBuffer );
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  generic TGRingBufferReverseEnumerator<T> = object
  protected
    FBuffer   : TRawRingBuffer;
    FPosition : Integer;
    function GetCurrent: T;
  public
    constructor Create( aBuffer : TRawRingBuffer );
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
    // Allows to be used as enumerator
    function GetEnumerator : TGRingBufferReverseEnumerator;
  end;

  generic TGRingBuffer<T> = class(TRawRingBuffer)
  public type
    TTypeRingBufferEnumerator        = specialize TGRingBufferEnumerator<T>;
    TTypeRingBufferReverseEnumerator = specialize TGRingBufferReverseEnumerator<T>;
    TTypeRingBuffer = array[0..(MaxInt div 1024)] of T;
    PTypeRingBuffer = ^TTypeRingBuffer;
    TType      = T;
    PType      = ^T;
  var protected
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
    function GetData : PTypeRingBuffer;
  public
    constructor Create( aCapacity : Integer );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    function PushFront( const aItem : T ) : Integer;
    function PushBack( const aItem : T ) : Integer;
    function PopFront : T;
    function PopBack : T;
    function Front : T;
    function Back : T;
    procedure Put( aIndex : Integer; const aItem : T );
    function Get( aIndex : Integer ) : T;
    function GetEnumerator: TTypeRingBufferEnumerator;
    function Reverse: TTypeRingBufferReverseEnumerator;

    property Items[ Index: Integer ] : T read Get write Put; default;
    property Data : PTypeRingBuffer  read GetData;
    property Size : Integer          read FCount;
    property Capacity : Integer      read FCapacity;
  end;

  generic TGObjectRingBuffer<T> = class(TRawRingBuffer)
  public type
    TTypeRingBufferEnumerator        = specialize TGRingBufferEnumerator<T>;
    TTypeRingBufferReverseEnumerator = specialize TGRingBufferReverseEnumerator<T>;
    TTypeRingBuffer = array[0..(MaxInt div 1024)] of T;
    PTypeRingBuffer = ^TTypeRingBuffer;
    TType      = T;
    PType      = ^T;
  var protected
    FManaged : Boolean;
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
    function GetData : PTypeRingBuffer;
  public
    constructor Create( aCapacity : Integer; aManaged : Boolean = True );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    function PushFront( const aItem : T ) : Integer;
    function PushBack( const aItem : T ) : Integer;
    function PopFront : T;
    function PopBack : T;
    function Front : T;
    function Back : T;
    procedure Put( aIndex : Integer; const aItem : T );
    function Get( aIndex : Integer ) : T;
    function GetEnumerator: TTypeRingBufferEnumerator;
    function Reverse: TTypeRingBufferReverseEnumerator;

    property Items[ Index: Integer ] : T read Get write Put; default;
    property Data : PTypeRingBuffer  read GetData;
    property Size : Integer          read FCount;
    property Capacity : Integer      read FCapacity;
  end;

  TRawPointerHeap = class( TRawPointerArray )
  protected
    FOnRawCompare : TRawPointerCompareFunc;
  protected
    procedure HeapPop;
    function HeapTop : Pointer;
    procedure HeapInsert( aItem : Pointer );
    function Greater( aIndex1, aIndex2 : Integer ) : Boolean;
    function Smaller( aIndex1, aIndex2 : Integer ) : Boolean;
    procedure HeapDown( aIndex : DWord );
    procedure HeapUp( aIndex : DWord );
  public
    constructor Create( aOnRawCompare : TRawPointerCompareFunc; aItemSize : Integer = sizeof(Pointer) );
    procedure Delete( aIndex : Integer ); override;
    function isEmpty : Boolean;
  end;

  generic TGHeap<T> = class( TRawPointerHeap )
  public type
    TTypeArrayEnumerator = specialize TGArrayEnumerator<T>;
    TCompareFunc         = function ( const aItem1, aItem2 : T ): Integer;
  var protected
    FOnCompare : TCompareFunc;
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
    function PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
  public
    constructor Create( aOnCompare : TCompareFunc );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    function Pop : T;
    function Top : T;
    function Get( aIndex : Integer ) : T;
    procedure Insert( const aItem : T );
    function GetEnumerator: TTypeArrayEnumerator;
  public
    property Items[ Index: Integer ] : T read Get; default;
    property OnCompare : TCompareFunc read FOnCompare write FOnCompare;
  end;

  generic TGObjectHeap<T> = class( TRawPointerHeap )
  public type
    TTypeArrayEnumerator = specialize TGArrayEnumerator<T>;
    TCompareFunc         = function ( const aItem1, aItem2 : T ): Integer;
  var protected
    FOnCompare : TCompareFunc;
    FManaged   : Boolean;
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
    function PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
  public
    constructor Create( aOnCompare : TCompareFunc; aManaged : Boolean = True );
    constructor CreateFromStream( Stream : TStream ); override;
    procedure WriteToStream( Stream : TStream ); override;
    function Pop : T;
    function Top : T;
    function Get( aIndex : Integer ) : T;
    procedure Insert( const aItem : T );
    function GetEnumerator: TTypeArrayEnumerator;
  public
    property Items[ Index: Integer ] : T read Get; default;
    property Managed : Boolean        read FManaged   write FManaged;
    property OnCompare : TCompareFunc read FOnCompare write FOnCompare;
  end;

  // A list for the purpose of choosing only a single element
  generic TGMinimalChoice<T> = class( TRawPointerArray )
    // Creates a new list
    constructor Create;
    // Adds a new value. If the value has lower priority then the best one yet
    // then all the ones added before are discarded.
    procedure Add( aValue : T; aPriority : DWord );
    // Returns a random value from the ones with the lowest priority
    function Return : T;
  protected
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
  protected
    FLowest  : DWord;
  end;

  // A list for the purpose of choosing only a single element
  generic TGMaximalChoice<T> = class( TRawPointerArray )
    // Creates a new list
    constructor Create;
    // Adds a new value. If the value has higher priority then the best one yet
    // then all the ones added before are discarded.
    procedure Add( aValue : T; aPriority : DWord );
    // Returns a random value from the ones with the lowest priority
    function Return : T;
  protected
    procedure CopyItem( aFrom, aTo : Pointer ); override;
    procedure DisposeOf( aItem : Pointer ); override;
  protected
    FHighest  : DWord;
  end;


implementation

uses typinfo;

{ TPointerVector }

procedure TRawPointerArray.CopyItem ( aFrom, aTo : Pointer ) ;
begin
  System.Move( aFrom^, aTo^, FItemSize );
end;

procedure TRawPointerArray.DisposeOf ( aItem : Pointer ) ;
begin
  // no-op
end;

procedure TRawPointerArray.DisposeOfRange ( aFromIndex, aToIndex : Integer ) ;
var Current, Stop : PByte;
begin
  Current := FData + aFromIndex * FItemSize;
  Stop    := FData + aToIndex   * FItemSize;
  while ( true ) do
  begin
    DisposeOf( Current );
    if Current = Stop then Break;
    Current += FItemSize;
  end;
end;

procedure TRawPointerArray.Expand;
begin
  if FCapacity = 0
    then SetCapacity( 4 )
    else SetCapacity( FCapacity * 2 );
end;

procedure TRawPointerArray.ExpandTo ( aCapacity : Integer ) ;
var NewCapacity : Integer;
begin
  NewCapacity := 4;
  while NewCapacity < aCapacity do NewCapacity *= 2;
  SetCapacity( NewCapacity );
end;

function TRawPointerArray.InternalPush ( aItem : Pointer ) : Integer;
begin
  if FCount = FCapacity then Expand;
  CopyItem( aItem, FData+FCount*FItemSize );
  Inc( FCount );
  Exit( FCount );
end;

function TRawPointerArray.InternalPop : Integer;
begin
  if FCount = 0 then raise ERangeError.Create('Pop on empty array called!');
  Dec( FCount );
  DisposeOf( FData + FCount*FItemSize  );
  Exit( FCount );
end;

function TRawPointerArray.InternalTop : Pointer;
begin
  if FCount = 0 then raise ERangeError.Create('Top on empty array called!');
  Exit( FData + (FCount-1)*FItemSize );
end;

procedure TRawPointerArray.InternalPut( aIndex : Integer; aItem : Pointer );
var Target : PByte;
begin
  if aIndex >= FCapacity then ExpandTo( aIndex+1 );
  if aIndex >= FCount then SetCount( aIndex+1 );
  Target := FData+aIndex*FItemSize;
  DisposeOf( Target );
  CopyItem( aItem, Target );
end;

function TRawPointerArray.InternalGet( aIndex : Integer ) : Pointer;
begin
  if aIndex >= FCount then raise ERangeError.Create('Get called out of array range!');
  Exit( FData+aIndex*FItemSize );
end;

procedure TRawPointerArray.InternalSwap( aIndex1, aIndex2 : Integer );
begin
  System.Move( (FData+aIndex1*FItemSize)^,   (FData+FCapacity*FItemSize)^, FItemSize);
  System.Move( (FData+aIndex2*FItemSize)^,   (FData+aIndex1*FItemSize)^,   FItemSize);
  System.Move( (FData+FCapacity*FItemSize)^, (FData+aIndex2*FItemSize)^,   FItemSize);
end;

function TRawPointerArray.InternalInsert( aIndex : Integer ) : Pointer;
begin
  if (aIndex < 0) or (aIndex >= FCount) then raise ERangeError.Create('Insert called out of array range!');
  if FCount = FCapacity then Expand;
  Result := FData+aIndex*FItemSize;
  if aIndex < FCount then
  begin
    System.Move( Result^, (Result+FItemSize)^, ( FCount - aIndex ) * FItemSize );
    System.FillByte( Result^, FItemSize, 0 );
  end;
  Inc( FCount );
end;

procedure TRawPointerArray.InternalInsert( aIndex : Integer; aItem : Pointer );
begin
  CopyItem( aItem, InternalInsert( aIndex ) );
end;

procedure TRawPointerArray.QuickSort( aL, aR : Integer; aCompare : TRawPointerCompareFunc );
var i, j, p : Integer;
    iPivot  : Pointer;
begin
  repeat
    i := aL;
    j := aR;
    p := (aL + aR) div 2;
    repeat
      iPivot := FData+P*FItemSize;
      while aCompare( iPivot, FData+i*FItemSize ) > 0 do Inc(i);
      while aCompare( iPivot, FData+j*FItemSize ) < 0 do Dec(j);
      if i <= j then
      begin
        InternalSwap( i, j );
        if p = i
          then p := j
          else if p = j
            then p := i;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if aL < j then
      QuickSort( aL, j, aCompare );
    aL := i;
  until i >= aR;
end;

constructor TRawPointerArray.Create ( aItemSize : Integer ) ;
begin
  inherited Create;
  FItemSize := aItemSize;
  FCount    := 0;
  FCapacity := 0;
  FData     := nil;
end;

constructor TRawPointerArray.CreateFromStream ( Stream : TStream ) ;
begin
  inherited CreateFromStream ( Stream ) ;
  FItemSize := Stream.ReadDWord;
  FCount    := 0;
  FCapacity := 0;
  FData     := nil;
  SetCapacity( Stream.ReadDWord );
end;

procedure TRawPointerArray.WriteToStream ( Stream : TStream ) ;
begin
  inherited WriteToStream ( Stream );
  Stream.WriteDWord( FItemSize );
  Stream.WriteDWord( FCapacity );
end;

destructor TRawPointerArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRawPointerArray.Clear;
begin
  if Assigned(FData) then
    SetCount( 0 );
end;

procedure TRawPointerArray.Reset;
begin
  if Assigned(FData) then
  begin
    SetCount( 0 );
    SetCapacity( 0 );
  end;
end;


procedure TRawPointerArray.PointerSort( aCompare : TRawPointerCompareFunc );
begin
  if FCount < 2 then Exit;
  QuickSort( 0, FCount - 1, aCompare );
end;

procedure TRawPointerArray.Delete( aIndex : Integer );
var iItem : Pointer;
begin
  if (aIndex < 0) or (aIndex >= FCount) then raise ERangeError.Create('Delete called out of array range!');
  Dec( FCount );
  iItem := FData+aIndex*FItemSize;
  DisposeOf( iItem );
  System.Move( (FData+(aIndex+1)*FItemSize)^, iItem^, ( FCount - aIndex ) * FItemSize );
  FillByte( (FData+FCount*FItemSize)^, FItemSize, 0 );
end;

procedure TRawPointerArray.Reserve ( aCapacity : Integer ) ;
begin
  if (aCapacity > FCapacity) then SetCapacity( aCapacity );
end;

procedure TRawPointerArray.Resize( aSize : Integer );
begin
  if aSize > FCapacity then ExpandTo( aSize + 1 );
  SetCount( aSize );
end;


function TRawPointerArray.IsEmpty : Boolean;
begin
  Exit( FCount = 0 );
end;

procedure TRawPointerArray.SetCapacity ( aCapacity : Integer ) ;
begin
  if (aCapacity < FCount) then raise ERangeError.Create('SetCapacity call lower than Count!');
  if (aCapacity = FCapacity) then Exit;
  if aCapacity <> 0 then Inc(aCapacity);
  ReallocMem( FData, aCapacity * FItemSize );
  FillChar( (FData + (FCapacity * FItemSize))^, ( aCapacity - FCapacity) * FItemSize, #0 );
  if aCapacity <> 0 then Dec(aCapacity);
  FCapacity := aCapacity;
end;

procedure TRawPointerArray.SetCount ( aCount : Integer ) ;
begin
  if (aCount < 0) {or (NewCount > MaxListSize)} then
    raise ERangeError.Create('SetCount out of range!');
  if aCount > FCapacity then SetCapacity( aCount );
  if aCount < FCount then
    DisposeOfRange(aCount, FCount-1);
  FCount := aCount;
end;

{ TGPArrayEnumerator }

function TGArrayEnumerator.GetCurrent : T;
begin
  Result := T(FArray.Items[FPosition]^);
end;

constructor TGArrayEnumerator.Create ( aArray : TRawPointerArray ) ;
begin
  FArray    := aArray;
  FPosition := -1;
end;

function TGArrayEnumerator.MoveNext : Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FArray.Size;
end;

{ TGArray }

procedure TGArray.CopyItem( aFrom, aTo : Pointer );
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGArray.DisposeOf ( aItem : Pointer );
begin
  Finalize(T(aItem^));
end;

function TGArray.PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
begin
  Result := FOnCompare(T(aItem1^), T(aItem2^));
end;

function TGArray.GetData : PTypeArray;
begin
  Exit( PTypeArray(FData) )
end;

constructor TGArray.Create;
begin
  inherited Create( SizeOf( T ) );
end;

constructor TGArray.CreateFromStream ( Stream : TStream ) ;
var iCount : DWord;
    iSize  : DWord;
begin
  inherited CreateFromStream ( Stream ) ;
  iSize := Stream.ReadDWord;
  SetCount( iSize );
  if iSize > 0 then
  for iCount := 0 to iSize - 1 do
    Stream.ReadType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

procedure TGArray.WriteToStream ( Stream : TStream ) ;
var iCount : DWord;
begin
  inherited WriteToStream ( Stream ) ;
  Stream.WriteDWord( FCount );
  if FCount > 0 then
  for iCount := 0 to FCount - 1 do
    Stream.WriteType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

procedure TGArray.Sort( aCompare : TCompareFunc );
begin
  FOnCompare := aCompare;
  PointerSort(@PointerCompare);
end;

function TGArray.Push ( const aItem : T ) : Integer;
begin
  Exit( InternalPush( @aItem ) );
end;

function TGArray.Pop : T;
begin
  Pop := T(InternalTop^);
  InternalPop;
end;

function TGArray.Top : T;
begin
  Exit( T(InternalTop^) );
end;

procedure TGArray.Put ( aIndex : Integer; const aItem : T ) ;
begin
  InternalPut(aIndex, @aItem);
end;

function TGArray.Get ( aIndex : Integer ) : T;
begin
  Exit( T(InternalGet( aIndex )^) );
end;

function TGArray.IndexOf ( const aItem : T ) : Integer;
begin
  IndexOf := 0;
  while (IndexOf < FCount) and (CompareByte(PType(FData)[IndexOf],aItem,FItemSize) <> 0) do
    Inc(IndexOf);
  if IndexOf = FCount then
    IndexOf := -1;
end;

function TGArray.GetEnumerator : TTypeArrayEnumerator;
begin
  GetEnumerator.Create( Self );
end;

{ TGObjectArray }

procedure TGObjectArray.CopyItem( aFrom, aTo : Pointer );
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGObjectArray.DisposeOf ( aItem : Pointer ) ;
begin
  if FManaged and (T(aItem^) <> T(nil)) then T( aItem^ ).Free;
end;

function TGObjectArray.GetData : PTypeArray;
begin
  Exit( PTypeArray(FData) );
end;

function TGObjectArray.PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
begin
  Result := FOnCompare(T(aItem1^), T(aItem2^));
end;

constructor TGObjectArray.Create ( aManaged : Boolean ) ;
begin
  inherited Create( SizeOf( T ) );
  FManaged := aManaged;
end;

constructor TGObjectArray.CreateFromStream ( Stream : TStream ) ;
var iCount : DWord;
    iSize  : DWord;
begin
  inherited CreateFromStream ( Stream ) ;
  FManaged := Stream.ReadByte <> 0;
  iSize := Stream.ReadDWord;
  SetCount( iSize );
  if iSize > 0 then
  for iCount := 0 to iSize - 1 do
    Stream.ReadType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

procedure TGObjectArray.WriteToStream ( Stream : TStream ) ;
var iCount : DWord;
begin
  inherited WriteToStream ( Stream ) ;
  if FManaged then Stream.WriteByte(1) else Stream.WriteByte(0);
  Stream.WriteDWord( FCount );
  if FCount > 0 then
  for iCount := 0 to FCount - 1 do
    Stream.WriteType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

procedure TGObjectArray.Sort( aCompare : TCompareFunc );
begin
  FOnCompare := aCompare;
  PointerSort(@PointerCompare);
end;

function TGObjectArray.Push ( const aItem : T ) : Integer;
begin
  Exit( InternalPush( @aItem ) );
end;

function TGObjectArray.Pop : T;
begin
  Pop := T(InternalTop^);
  InternalPop;
  if FManaged then Exit( T(nil) );
end;

function TGObjectArray.Top : T;
begin
  Exit( T(InternalTop^) );
end;

procedure TGObjectArray.Put ( aIndex : Integer; const aItem : T ) ;
begin
  InternalPut( aIndex, @aItem );
end;

function TGObjectArray.Get ( aIndex : Integer ) : T;
begin
  Exit( T(InternalGet( aIndex )^) );
end;

{$HINTS OFF}
function TGObjectArray.IndexOf ( const aItem : T ) : Integer;
begin
  IndexOf := 0;
  while (IndexOf < FCount) and (PType(FData)[IndexOf] <> aItem) do
    Inc(IndexOf);
  if IndexOf = FCount then
    IndexOf := -1;
end;
{$HINTS ON}

function TGObjectArray.GetEnumerator : TTypeArrayEnumerator;
begin
  GetEnumerator.Create( Self );
end;

{ TRawHashMapBucket }

constructor TRawHashMapBucket.Create ( aHashMap : TRawHashMap ) ;
begin
  inherited Create;
  FHashMap  := aHashMap;
  FValues   := nil;
  FCount    := 0;
  FCapacity := 0;
  FItemSize := aHashMap.ItemSize;
end;

procedure TRawHashMapBucket.Add ( const aKey : AnsiString; aValue : Pointer ) ;
var NewCapacity : Integer;
begin
  if FCount = FCapacity then
  begin
    if FCapacity = 0
      then NewCapacity := 4
      else NewCapacity := 2*FCapacity;
    ReallocMem( FValues, NewCapacity*FItemSize );
    SetLength( FKeys, NewCapacity );
    FillChar( (FValues + (FCapacity * FItemSize))^, ( NewCapacity - FCapacity) * FItemSize, #0 );
    FCapacity := NewCapacity;
  end;
  FKeys[ FCount ] := aKey;
  FHashMap.CopyItem(aValue, FValues+FCount*FItemSize);
  Inc(FCount);
end;

function TRawHashMapBucket.FindIndex ( const aKey : AnsiString ) : Integer;
var Idx : DWord;
begin
  FindIndex := -1;
  if FCount > 0 then
    for Idx := 0 to FCount-1 do
      if FKeys[ Idx ] = aKey then
        Exit( Idx );
end;

function TRawHashMapBucket.GetValue ( const aKey : AnsiString ) : Pointer;
var Idx : Integer;
begin
  if FCount > 0 then
    for Idx := 0 to FCount-1 do
      if FKeys[ Idx ] = aKey then
        Exit( FValues + Idx * FItemSize );
  Exit( nil );
end;

function TRawHashMapBucket.GetValue ( aIndex : Integer ) : Pointer;
begin
  Exit( FValues + aIndex * FItemSize );
end;

procedure TRawHashMapBucket.SetValue ( aIndex : Integer; aValue : Pointer );
begin
  FHashMap.CopyItem( aValue, FValues+aIndex*FItemSize );
end;

function TRawHashMapBucket.Remove ( const aKey : AnsiString ) : Boolean;
var Idx : Integer;
begin
  Idx := FindIndex( aKey );
  if Idx = -1 then Exit( False );
  Dec( FCount );
  FHashMap.DisposeOf( FValues+Idx*FItemSize  );
  if Idx <> FCount then
  begin
    System.Move( (FValues+FCount*FItemSize)^, (FValues+Idx*FItemSize)^, FItemSize );
    FKeys[ Idx ] := FKeys[ FCount ];
  end;
  Exit( True );
end;

function TRawHashMapBucket.GetKey ( aIndex : Integer ) : AnsiString;
begin
  Exit( FKeys[ aIndex ] );
end;

destructor TRawHashMapBucket.Destroy;
var Idx : Integer;
begin
  if FCount > 0 then
  begin
    for Idx := 0 to FCount-1 do
      FHashMap.DisposeOf( FValues + Idx * FItemSize );
  end;
  FreeMem( FValues );
  inherited Destroy;
end;

{ TRawHashMap }

procedure TRawHashMap.CopyItem ( aFrom, aTo : Pointer ) ;
begin
  System.Move(aFrom^, aTo^, FItemSize);
end;

procedure TRawHashMap.DisposeOf ( aItem : Pointer ) ;
begin
  // noop
end;

constructor TRawHashMap.Create ( aPolicy : THashMapPolicy; aBuckets : Integer; aItemSize : Integer ) ;
var Idx : Integer;
begin
  FPolicy    := aPolicy;
  FCount     := 0;
  FItemSize  := aItemSize;
  FBuckets   := aBuckets;
  FLastQuery := '';
  FLastValue := nil;

  SetLength( FBucket, FBuckets );
  for Idx := 0 to FBuckets-1 do
    FBucket[ Idx ] := nil;
end;

constructor TRawHashMap.CreateFromStream( Stream : TStream );
var iCount : Integer;
begin
  inherited;
  FPolicy    := THashMapPolicy( Stream.ReadByte );
  FCount     := 0;
  FItemSize  := Stream.ReadDWord;
  FBuckets   := Stream.ReadDWord;
  FLastQuery := '';
  FLastValue := nil;

  SetLength( FBucket, FBuckets );
  for iCount := 0 to FBuckets-1 do
    FBucket[ iCount ] := nil;
end;

procedure TRawHashMap.WriteToStream( Stream : TStream );
begin
  inherited;
  Stream.WriteByte( Byte(FPolicy) );
  Stream.WriteDWord( FItemSize );
  Stream.WriteDWord( FBuckets );
end;

procedure TRawHashMap.Clear;
var Idx : Integer;
begin
  for Idx := 0 to FBuckets-1 do
    FreeAndNil( FBucket[ Idx ] );
  FCount     := 0;
  FLastQuery := '';
  FLastValue := nil;
end;

destructor TRawHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TRawHashMap.LinearGet( aBIdx, aIIdx : Integer ) : Pointer;
begin
  Exit( FBucket[ aBIdx ].GetValue( aIIdx ) );
end;

function TRawHashMap.LinearGetKey( aBIdx, aIIdx : Integer ) : AnsiString;
begin
  Exit( FBucket[ aBIdx ].GetKey( aIIdx ) );
end;


function TRawHashMap.BucketSize( aBIdx : Integer ) : Integer;
begin
  if FBucket[ aBIdx ] = nil then Exit( 0 );
  Exit( FBucket[ aBIdx ].Count );
end;

function TRawHashMap.Exists ( const aKey : AnsiString ) : Boolean;
begin
  Exit( Query( aKey ) <> nil );
end;

function TRawHashMap.Remove ( const aKey : AnsiString ) : Boolean;
var BIdx : Integer;
begin
  BIdx := Hash( aKey );
  if FBucket[ BIdx ] = nil then Exit( false );
  if FBucket[ BIdx ].Remove( aKey ) then
  begin
    Dec( FCount );
    Exit( True );
  end
  else
    Exit( False );
end;

function TRawHashMap.Query( const aKey : AnsiString ) : Pointer;
var BIdx : Integer;
begin
  if aKey = FLastQuery then Exit( FLastValue );
  BIdx := Hash( aKey );
  if FBucket[ BIdx ] = nil then Exit( nil );
  FLastValue := FBucket[ BIdx ].GetValue( aKey );
  Exit( FLastValue );
end;

function TRawHashMap.Hash ( const aKey : AnsiString ) : Integer;
var i : Byte;
begin
  FLastQuery := aKey;
  FLastValue := nil;
  Hash := 0;
  if Length( aKey ) > 0 then
  for i := 1 to Length(aKey) do
    Hash := ( Hash * 31 + Ord(aKey[i]) ) mod $FFFF;
  Hash := Hash mod FBuckets;
end;

function TRawHashMap.InternalAdd ( const aKey : AnsiString; aValue : Pointer ) : Boolean;
var BIdx : Integer;
    IIdx : Integer;
begin
  BIdx := Hash( aKey );
  FLastQuery := '';
  if FBucket[ BIdx ] = nil then
  begin
    FBucket[ BIdx ] := TRawHashMapBucket.Create(Self);
    IIdx := -1;
  end
  else
    IIdx := FBucket[ BIdx ].FindIndex( aKey );
  if IIdx <> -1 then
  begin
    if (FPolicy = HashMap_RaiseAll) or (FPolicy = HashMap_RaiseCollision)
      then raise ECollisionError.Create('Key '+aKey+' already exists in HashMap!')
      else
      begin
        FBucket[ BIdx ].SetValue( IIdx, aValue );
        Exit( False );
      end;
  end
  else
  begin
    Inc( FCount );
    FBucket[ BIdx ].Add( aKey, aValue );
    Exit( True );
  end;
end;

function TGHashMapEnumerator.GetCurrent: TPairType;
begin
  GetCurrent.Key   := FKey;
  GetCurrent.Value := T(FCurrent^);
end;

constructor TGHashMapEnumerator.Create( aHashMap : TRawHashMap );
begin
  FHashMap := aHashMap;
  FBIdx    := -1;
  FIIdx    := -1;
  FISize   := 0;
end;

function TGHashMapEnumerator.MoveNext: Boolean;
begin
  Inc( FIIdx );
  if FIIdx = FISize then
  begin
    FISize := 0;
    FIIdx  := 0;
  end;
  while FISize = 0 do
  begin
    Inc(FBIdx);
    if FBIdx = FHashMap.Buckets then Exit( False );
    FISize := FHashMap.BucketSize( FBIdx );
  end;
  FCurrent := FHashMap.LinearGet( FBIdx, FIIdx );
  FKey     := FHashMap.LinearGetKey( FBIdx, FIIdx );
  Exit( True );
end;

procedure TGHashMap.CopyItem( aFrom, aTo : Pointer );
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGHashMap.DisposeOf( aItem : Pointer );
begin
  Finalize(T(aItem^));
end;

constructor TGHashMap.Create( aPolicy : THashMapPolicy = HashMap_NoRaise; aBuckets : Integer = 94 );
begin
  inherited Create( aPolicy, aBuckets, SizeOf(T) );
end;

constructor TGHashMap.CreateFromStream( Stream : TStream );
var iCount, iSize : DWord;
    iKey          : AnsiString;
    iElement      : T;
begin
  inherited;
  iSize := Stream.ReadDWord;
  if iSize > 0 then
  for iCount := 0 to iSize-1 do
  begin
    iKey := Stream.ReadAnsiString;
    Stream.ReadType( @iElement, SizeOf(T), TypeInfo(T) );
    InternalAdd( iKey, @iElement );
  end;
end;

procedure TGHashMap.WriteToStream( Stream : TStream );
var iBucket, iCount, iSize : DWord;
begin
  inherited;
  Stream.WriteDWord( FCount );
  for iBucket := 0 to FBuckets-1 do
  begin
    iSize := BucketSize( iBucket );
    if iSize > 0 then
    for iCount := 0 to iSize-1 do
    begin
      Stream.WriteAnsiString( FBucket[ iBucket ].GetKey( iCount ) );
      Stream.WriteType( FBucket[ iBucket ].GetValue( iCount ), SizeOf(T), TypeInfo(T) );
    end;
  end;
end;

procedure TGHashMap.Put( const aKey : AnsiString; const aValue : T );
begin
  InternalAdd( aKey, @aValue );
end;

function TGHashMap.Get( const aKey : AnsiString ) : T;
var Ptr : Pointer;
begin
  Ptr := Query( aKey );
  if Ptr <> nil
    then Exit( T(Ptr^) )
    else
    if (FPolicy = HashMap_RaiseAll) or (FPolicy = HashMap_RaiseUndefined)
      then raise EUndefinedError.Create('Key '+aKey+' undefined in HashMap!')
      else FillByte(Get, sizeof(T), 0);
end;

function TGHashMap.Get( const aKey : AnsiString; const DefVal : T ) : T;
var Ptr : Pointer;
begin
  Ptr := Query( aKey );
  if Ptr <> nil
    then Exit( T(Ptr^) )
    else Exit( DefVal );
end;

function TGHashMap.GetEnumerator: TTypeHashMapEnumerator;
begin
  GetEnumerator := TTypeHashMapEnumerator.Create( Self );
end;

procedure TGObjectHashMap.CopyItem( aFrom, aTo : Pointer );
begin
  if FManaged and (T(aTo^) <> T(nil)) then T( aTo^ ).Free;
  T(aTo^) := T(aFrom^);
end;

procedure TGObjectHashMap.DisposeOf( aItem : Pointer );
begin
  if FManaged and (T(aItem^) <> T(nil)) then
    T(aItem^).Free;
end;

constructor TGObjectHashMap.Create( aManaged : Boolean = True; aPolicy : THashMapPolicy = HashMap_NoRaise; aBuckets : Integer = 94 );
begin
  inherited Create( aPolicy, aBuckets, SizeOf(T) );
  FManaged := aManaged;
end;

constructor TGObjectHashMap.CreateFromStream( Stream : TStream );
var iCount, iSize : DWord;
    iKey          : AnsiString;
    iElement      : T;
begin
  inherited;
  FManaged := Stream.ReadByte <> 0;
  iSize := Stream.ReadDWord;
  if iSize > 0 then
  for iCount := 0 to iSize-1 do
  begin
    iKey := Stream.ReadAnsiString;
    Stream.ReadType( @iElement, SizeOf(T), TypeInfo(T) );
    InternalAdd( iKey, @iElement );
  end;
end;

procedure TGObjectHashMap.WriteToStream( Stream : TStream );
var iBucket, iCount, iSize : DWord;
begin
  inherited;
  if FManaged then Stream.WriteByte(1) else Stream.WriteByte(0);
  Stream.WriteDWord( FCount );
  for iBucket := 0 to FBuckets-1 do
  begin
    iSize := BucketSize( iBucket );
    if iSize > 0 then
    for iCount := 0 to iSize-1 do
    begin
      Stream.WriteAnsiString( FBucket[ iBucket ].GetKey( iCount ) );
      Stream.WriteType( FBucket[ iBucket ].GetValue( iCount ), SizeOf(T), TypeInfo(T) );
    end;
  end;
end;

procedure TGObjectHashMap.Put( const aKey : AnsiString; const aValue : T );
begin
  InternalAdd( aKey, @aValue );
end;

function TGObjectHashMap.Get( const aKey : AnsiString ) : T;
var Ptr : Pointer;
begin
  Ptr := Query( aKey );
  if Ptr <> nil
    then Exit( T(Ptr^) )
    else if (FPolicy = HashMap_RaiseAll) or (FPolicy = HashMap_RaiseUndefined)
      then raise EUndefinedError.Create('Key '+aKey+' undefined in HashMap!')
      else Exit( T(nil) );
end;

function TGObjectHashMap.GetEnumerator: TTypeHashMapEnumerator;
begin
  GetEnumerator := TTypeHashMapEnumerator.Create( Self );
end;

{ TRawRingBuffer }

procedure TRawRingBuffer.CopyItem ( aFrom, aTo : Pointer ) ;
begin
  System.Move( aFrom^, aTo^, FItemSize );
end;

procedure TRawRingBuffer.DisposeOf ( aItem : Pointer ) ;
begin
  // no-op
end;

procedure TRawRingBuffer.DisposeOfRange ( aFromIndex, aToIndex : Integer ) ;
var Current, Switch, Stop : PByte;
begin
  Current := FData + aFromIndex    * FItemSize;
  Stop    := FData + aToIndex      * FItemSize;
  Switch  := FData + (FCapacity-1) * FItemSize;
  while ( true ) do
  begin
    DisposeOf( Current );
    if Current = Stop then Break;
    if Current = Switch
      then Current := FData
      else Current += FItemSize;
  end;
end;

function TRawRingBuffer.InternalPushFront ( aItem : Pointer ) : Integer;
var Position : Integer;
begin
  if FStart = 0
    then Position := FCapacity-1
    else Position := FStart-1;
  CopyItem( aItem, FData+Position*FItemSize );
  FStart := Position;
  if FCount <> FCapacity then
    Inc( FCount );
  Exit( FCount );
end;

function TRawRingBuffer.InternalPushBack ( aItem : Pointer ) : Integer;
var Position : Integer;
begin
  Position := ( FStart + FCount ) mod FCapacity;
  CopyItem( aItem, FData+Position*FItemSize );
  if FCount = FCapacity then
    FStart := (FStart + 1) mod FCapacity
  else
    Inc( FCount );
  Exit( FCount );
end;

function TRawRingBuffer.InternalPopFront : Integer;
var Position : Integer;
begin
  if FCount = 0 then raise ERangeError.Create('PopFront on empty array called!');
  Dec( FCount );
  Position := ( FStart + 1 ) mod FCapacity;
  DisposeOf( FData + FStart*FItemSize  );
  FStart := Position;
  Exit( FCount );
end;

function TRawRingBuffer.InternalPopBack : Integer;
var Position : Integer;
begin
  if FCount = 0 then raise ERangeError.Create('PopBack on empty array called!');
  Dec( FCount );
  Position := ( FStart + FCount ) mod FCapacity;
  DisposeOf( FData + Position*FItemSize  );
  Exit( FCount );
end;


function TRawRingBuffer.InternalFront : Pointer;
begin
  if FCount = 0 then raise ERangeError.Create('Front on empty array called!');
  Exit( FData + FStart*FItemSize );
end;

function TRawRingBuffer.InternalBack : Pointer;
begin
  if FCount = 0 then raise ERangeError.Create('Front on empty array called!');
  Exit( FData + ( ( FStart + FCount - 1 ) mod FCapacity ) * FItemSize );
end;

procedure TRawRingBuffer.InternalPut ( aIndex : Integer; aItem : Pointer ) ;
var Position : Integer;
begin
  if aIndex >= 0 then
  begin
    if aIndex > FCount then ERangeError.Create('Put index out of range!');
    Position := ( FStart + aIndex ) mod FCapacity;
  end
  else
  begin
    if -aIndex > FCount then ERangeError.Create('Put index out of range!');
    Position := ( FStart + FCount + aIndex ) mod FCapacity;
  end;
  CopyItem( aItem, FData + Position*FItemSize );
end;

function TRawRingBuffer.InternalGet ( aIndex : Integer ) : Pointer;
var Position : Integer;
begin
  if aIndex >= 0 then
  begin
    if aIndex > FCount then ERangeError.Create('Get index out of range!');
    Position := ( FStart + aIndex ) mod FCapacity;
  end
  else
  begin
    if -aIndex > FCount then ERangeError.Create('Get index out of range!');
    Position := ( FStart + FCount + aIndex ) mod FCapacity;
  end;
  Exit( FData+Position*FItemSize );
end;

constructor TRawRingBuffer.Create ( aCapacity : Integer; aItemSize : Integer ) ;
begin
  FItemSize := aItemSize;
  FCount    := 0;
  FStart    := 0;
  FCapacity := aCapacity;
  FData     := nil;
  ReallocMem( FData, FCapacity * FItemSize );
  FillChar( FData^, FCapacity * FItemSize, #0 );
end;

constructor TRawRingBuffer.CreateFromStream( Stream : TStream );
begin
  inherited;
  FItemSize := Stream.ReadDWord;
  FCount    := 0;
  FStart    := 0;
  FCapacity := Stream.ReadDWord;
  FData     := nil;
  ReallocMem( FData, FCapacity * FItemSize );
  FillChar( FData^, FCapacity * FItemSize, #0 );
end;

procedure TRawRingBuffer.WriteToStream( Stream : TStream );
begin
  inherited;
  Stream.WriteDWord( FItemSize );
  Stream.WriteDWord( FCapacity );
end;

destructor TRawRingBuffer.Destroy;
begin
  Clear;
  ReallocMem( FData, 0 );
  inherited Destroy;
end;

procedure TRawRingBuffer.Clear;
begin
  if FCount > 0 then
  begin
    DisposeOfRange( FStart, ( FStart + FCount - 1 ) mod FCapacity );
    FCount    := 0;
    FStart    := 0;
  end;
end;

{ TGRingBufferEnumerator }

function TGRingBufferEnumerator.GetCurrent : T;
begin
  Result := T(FBuffer.Items[FPosition]^);
end;

constructor TGRingBufferEnumerator.Create ( aBuffer : TRawRingBuffer ) ;
begin
  FBuffer   := aBuffer;
  FPosition := -1;
end;

function TGRingBufferEnumerator.MoveNext : Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FBuffer.Size;
end;

{ TGRingBufferReverseEnumerator }

function TGRingBufferReverseEnumerator.GetCurrent : T;
begin
  Result := T(FBuffer.Items[FPosition]^);
end;

constructor TGRingBufferReverseEnumerator.Create ( aBuffer : TRawRingBuffer ) ;
begin
  FBuffer := aBuffer;
  FPosition := 0;
end;

function TGRingBufferReverseEnumerator.MoveNext : Boolean;
begin
  Dec(FPosition);
  Result := -FPosition <= FBuffer.Size;
end;

function TGRingBufferReverseEnumerator.GetEnumerator : TGRingBufferReverseEnumerator;
begin
  Exit( Self );
end;

{ TGRingBuffer }

procedure TGRingBuffer.CopyItem ( aFrom, aTo : Pointer ) ;
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGRingBuffer.DisposeOf ( aItem : Pointer ) ;
begin
  Finalize(T(aItem^));
end;

function TGRingBuffer.GetData : PTypeRingBuffer;
begin
  Exit( PTypeRingBuffer(FData) )
end;

constructor TGRingBuffer.Create ( aCapacity : Integer ) ;
begin
  inherited Create( aCapacity, SizeOf(T) );
end;

constructor TGRingBuffer.CreateFromStream( Stream : TStream );
var iCount : LongInt;
begin
  inherited;
  FCount    := Stream.ReadDWord;
  FStart    := Stream.ReadDWord;
  for iCount := 0 to FCapacity-1 do
    Stream.ReadType( FData+iCount*FItemSize, SizeOf(T), TypeInfo(T) );
end;

procedure TGRingBuffer.WriteToStream( Stream : TStream );
var iCount : LongInt;
begin
  inherited;
  Stream.WriteDWord( FCount );
  Stream.WriteDWord( FStart );
  for iCount := 0 to FCapacity-1 do
    Stream.WriteType( FData+iCount*FItemSize, SizeOf(T), TypeInfo(T) );
end;

function TGRingBuffer.PushFront ( const aItem : T ) : Integer;
begin
  Exit( InternalPushFront( @aItem ) );
end;

function TGRingBuffer.PushBack ( const aItem : T ) : Integer;
begin
  Exit( InternalPushBack( @aItem ) );
end;

function TGRingBuffer.PopFront : T;
begin
  PopFront := T(InternalFront^);
  InternalPopFront;
end;

function TGRingBuffer.PopBack : T;
begin
  PopBack := T(InternalBack^);
  InternalPopBack;
end;

function TGRingBuffer.Front : T;
begin
  Exit( T(InternalFront^) );
end;

function TGRingBuffer.Back : T;
begin
  Exit( T(InternalBack^) );
end;

procedure TGRingBuffer.Put ( aIndex : Integer; const aItem : T ) ;
begin
  InternalPut(aIndex, @aItem);
end;

function TGRingBuffer.Get ( aIndex : Integer ) : T;
begin
  Exit( T(InternalGet( aIndex )^) );
end;

function TGRingBuffer.GetEnumerator : TTypeRingBufferEnumerator;
begin
  GetEnumerator.Create( Self );
end;

function TGRingBuffer.Reverse : TTypeRingBufferReverseEnumerator;
begin
  Reverse.Create( Self );
end;

{ TGObjectRingBuffer }

procedure TGObjectRingBuffer.CopyItem ( aFrom, aTo : Pointer ) ;
begin
  if FManaged and (T(aTo^) <> T(nil)) then T( aTo^ ).Free;
  T(aTo^) := T(aFrom^);
end;

procedure TGObjectRingBuffer.DisposeOf ( aItem : Pointer ) ;
begin
  if FManaged and (T(aItem^) <> T(nil)) then T( aItem^ ).Free;
end;

function TGObjectRingBuffer.GetData : PTypeRingBuffer;
begin
  Exit( PTypeRingBuffer(FData) );
end;

constructor TGObjectRingBuffer.Create ( aCapacity : Integer; aManaged : Boolean ) ;
begin
  inherited Create( aCapacity, SizeOf( T ) );
  FManaged := aManaged;
end;

constructor TGObjectRingBuffer.CreateFromStream( Stream : TStream );
var iCount : LongInt;
begin
  inherited;
  FManaged  := Stream.ReadByte <> 0;
  FCount    := Stream.ReadDWord;
  FStart    := Stream.ReadDWord;
  for iCount := 0 to FCapacity-1 do
    Stream.ReadType( FData+iCount*FItemSize, SizeOf(T), TypeInfo(T) );
end;

procedure TGObjectRingBuffer.WriteToStream( Stream : TStream );
var iCount : LongInt;
begin
  inherited;
  if FManaged then Stream.WriteByte(1) else Stream.WriteByte(0);
  Stream.WriteDWord( FCount );
  Stream.WriteDWord( FStart );
  for iCount := 0 to FCapacity-1 do
    Stream.WriteType( FData+iCount*FItemSize, SizeOf(T), TypeInfo(T) );
end;

function TGObjectRingBuffer.PushFront ( const aItem : T ) : Integer;
begin
  Exit( InternalPushFront( @aItem ) );
end;

function TGObjectRingBuffer.PushBack ( const aItem : T ) : Integer;
begin
  Exit( InternalPushBack( @aItem ) );
end;

function TGObjectRingBuffer.PopFront : T;
begin
  PopFront := T(InternalFront^);
  InternalPopFront;
  if FManaged then Exit( T(nil) );
end;

function TGObjectRingBuffer.PopBack : T;
begin
  PopBack := T(InternalBack^);
  InternalPopBack;
  if FManaged then Exit( T(nil) );
end;

function TGObjectRingBuffer.Front : T;
begin
  Exit( T(InternalFront^) );
end;

function TGObjectRingBuffer.Back : T;
begin
  Exit( T(InternalBack^) );
end;

procedure TGObjectRingBuffer.Put ( aIndex : Integer; const aItem : T ) ;
begin
  InternalPut(aIndex, @aItem);
end;

function TGObjectRingBuffer.Get ( aIndex : Integer ) : T;
begin
  Exit( T(InternalGet( aIndex )^) );
end;

function TGObjectRingBuffer.GetEnumerator : TTypeRingBufferEnumerator;
begin
  GetEnumerator.Create( Self );
end;

function TGObjectRingBuffer.Reverse : TTypeRingBufferReverseEnumerator;
begin
  Reverse.Create( Self );
end;

procedure TRawPointerHeap.HeapPop;
begin
  if FCount = 0 then raise ERangeError.Create('Pop on empty heap called!');
  Dec( FCount );
  DisposeOf( FData );
  if FCount = 0 then Exit;
  CopyItem( FData+FCount*FItemSize, FData );
  HeapDown(0);
end;

function TRawPointerHeap.HeapTop : Pointer;
begin
  if FCount = 0 then raise ERangeError.Create('Top on empty heap called!');
  Exit( FData );
end;

procedure TRawPointerHeap.HeapInsert( aItem : Pointer );
begin
  InternalPush( aItem );
  HeapUp( FCount - 1 );
end;

function TRawPointerHeap.Greater( aIndex1, aIndex2 : Integer ) : Boolean;
begin
  if (aIndex1 = aIndex2) or (aIndex2 >= FCount) then Exit(False);
  Greater := FOnRawCompare( FData + aIndex1*FItemSize, FData + aIndex2*FItemSize ) > 0;
end;

function TRawPointerHeap.Smaller( aIndex1, aIndex2 : Integer ) : Boolean;
begin
  if aIndex1 = aIndex2 then Exit(False);
  Smaller := FOnRawCompare( FData + aIndex1*FItemSize, FData + aIndex2*FItemSize ) < 0;
end;

procedure TRawPointerHeap.HeapDown( aIndex : DWord );
var l : LongInt;
begin
  // go down with Data[0]
  repeat
    l := (aIndex+1)*2-1;
    if l >= FCount then Break;
    if l+1 < FCount then // not last
      if Greater(l+1,l) then Inc(l);
    if Greater(l,aIndex) then
    begin
      InternalSwap(l,aIndex);
      aIndex := l;
    end else Break;
  until false;
end;


procedure TRawPointerHeap.HeapUp( aIndex : DWord );
begin
  // go up with Data[Entries]
  while (aIndex <> 0) and Smaller((aIndex - 1) div 2,aIndex) do
  begin
    InternalSwap((aIndex - 1) div 2,aIndex);
    aIndex := (aIndex - 1) div 2;
  end;
end;

constructor TRawPointerHeap.Create( aOnRawCompare : TRawPointerCompareFunc; aItemSize : Integer = sizeof(Pointer) );
begin
  inherited Create( aItemSize );
  FOnRawCompare := aOnRawCompare;
end;

procedure TRawPointerHeap.Delete( aIndex : Integer );
begin
  if aIndex = 0 then begin HeapPop; exit; end;
  if aIndex >= FCount then raise ERangeError.Create('Bad index passed to TGHeap.Delete!');
  Dec( FCount );
  DisposeOf( FData+aIndex*FItemSize );
  InternalSwap( aIndex, FCount );
  if aIndex = FCount then Exit;
  if FCount = 1 then Exit;

  if Smaller((aIndex - 1) div 2,aIndex)
    then HeapUp( aIndex )
    else HeapDown( aIndex );
end;


function TRawPointerHeap.isEmpty : Boolean;
begin
  Exit( FCount = 0 );
end;

function TGHeap.PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
begin
  Result := FOnCompare(T(aItem1^), T(aItem2^));
end;

procedure TGHeap.CopyItem( aFrom, aTo : Pointer );
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGHeap.DisposeOf( aItem : Pointer );
begin
  Finalize( T(aItem^) );
end;

constructor TGHeap.Create( aOnCompare : TCompareFunc );
begin
  FOnCompare    := aOnCompare;
  inherited Create( @PointerCompare, SizeOf( T ) );
end;

constructor TGHeap.CreateFromStream ( Stream : TStream ) ;
var iCount : DWord;
    iSize  : DWord;
begin
  inherited CreateFromStream ( Stream ) ;
  FOnRawCompare := @PointerCompare;
  FOnCompare    := nil;
  iSize := Stream.ReadDWord;
  SetCount( iSize );
  if iSize > 0 then
  for iCount := 0 to iSize - 1 do
    Stream.ReadType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

procedure TGHeap.WriteToStream ( Stream : TStream ) ;
var iCount : DWord;
begin
  inherited WriteToStream ( Stream ) ;
  Stream.WriteDWord( FCount );
  if FCount > 0 then
  for iCount := 0 to FCount - 1 do
    Stream.WriteType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

function TGHeap.Pop : T;
begin
  Result := T( HeapTop^ );
  HeapPop;
end;

function TGHeap.Top : T;
begin
  Result := T( HeapTop^ );
end;

function TGHeap.Get( aIndex : Integer ) : T;
begin
  Result := T( InternalGet( aIndex )^ );
end;

procedure TGHeap.Insert( const aItem : T );
begin
  HeapInsert( @aItem );
end;

function TGHeap.GetEnumerator : TTypeArrayEnumerator;
begin
  GetEnumerator.Create( Self );
end;

function TGObjectHeap.PointerCompare( aItem1, aItem2 : Pointer ) : Integer;
begin
  Result := FOnCompare(T(aItem1^), T(aItem2^));
end;

procedure TGObjectHeap.CopyItem( aFrom, aTo : Pointer );
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGObjectHeap.DisposeOf( aItem : Pointer );
begin
  if FManaged and (T(aItem^) <> T(nil)) then T( aItem^ ).Free;
end;

constructor TGObjectHeap.Create( aOnCompare : TCompareFunc; aManaged : Boolean = True );
begin
  FOnCompare    := aOnCompare;
  FManaged      := aManaged;
  inherited Create( @PointerCompare, SizeOf( T ) );
end;

constructor TGObjectHeap.CreateFromStream ( Stream : TStream ) ;
var iCount : DWord;
    iSize  : DWord;
begin
  inherited CreateFromStream ( Stream ) ;
  FOnRawCompare := @PointerCompare;
  FOnCompare    := nil;
  FManaged      := Stream.ReadByte <> 0;
  iSize := Stream.ReadDWord;
  SetCount( iSize );
  if iSize > 0 then
  for iCount := 0 to iSize - 1 do
    Stream.ReadType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

procedure TGObjectHeap.WriteToStream ( Stream : TStream ) ;
var iCount : DWord;
begin
  inherited WriteToStream ( Stream ) ;
  if FManaged then Stream.WriteByte(1) else Stream.WriteByte(0);
  Stream.WriteDWord( FCount );
  if FCount > 0 then
  for iCount := 0 to FCount - 1 do
    Stream.WriteType( InternalGet( iCount ), SizeOf( T ), TypeInfo( T ) );
end;

function TGObjectHeap.Pop : T;
begin
  Result := T( HeapTop^ );
  HeapPop;
  if FManaged then Exit( T( nil ) );
end;

function TGObjectHeap.Top : T;
begin
  Result := T( HeapTop^ );
end;

function TGObjectHeap.Get( aIndex : Integer ) : T;
begin
  Result := T( InternalGet( aIndex )^ );
end;

procedure TGObjectHeap.Insert( const aItem : T );
begin
  HeapInsert( @aItem );
end;

function TGObjectHeap.GetEnumerator : TTypeArrayEnumerator;
begin
  GetEnumerator.Create( Self );
end;


constructor TGMinimalChoice.Create;
begin
  inherited Create( SizeOf( T ) );
  FLowest := $FFFFFFFF;
end;

procedure TGMinimalChoice.Add( aValue : T; aPriority : DWord );
begin
  if aPriority > FLowest then Exit;
  if aPriority < FLowest then
  begin
    FLowest  := aPriority;
    Clear;
  end;
  InternalPush( @aValue );
end;

function TGMinimalChoice.Return : T;
begin
  if FCount = 0 then raise ERangeError.Create('Return called on a empty TGMinimalList!');
  Exit(T(InternalGet(Random(FCount))^));
end;

procedure TGMinimalChoice.CopyItem( aFrom, aTo : Pointer );
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGMinimalChoice.DisposeOf( aItem : Pointer );
begin
  Finalize(T(aItem^));
end;

constructor TGMaximalChoice.Create;
begin
  inherited Create( SizeOf( T ) );
  FHighest := 0;
end;

procedure TGMaximalChoice.Add( aValue : T; aPriority : DWord );
begin
  if aPriority < FHighest then Exit;
  if aPriority > FHighest then
  begin
    FHighest := aPriority;
    Clear;
  end;
  InternalPush( @aValue );
end;

function TGMaximalChoice.Return : T;
begin
  if FCount = 0 then raise ERangeError.Create('Return called on a empty TGMinimalList!');
  Exit(T(InternalGet(Random(FCount))^));
end;

procedure TGMaximalChoice.CopyItem( aFrom, aTo : Pointer );
begin
  T(aTo^) := T(aFrom^);
end;

procedure TGMaximalChoice.DisposeOf( aItem : Pointer );
begin
  Finalize(T(aItem^));
end;

end.

