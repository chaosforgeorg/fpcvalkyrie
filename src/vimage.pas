{$include valkyrie.inc}
unit vimage;
interface
uses vcolor;

type

{ TImage }

TImage = class
    constructor Create( aSizeX, aSizeY : DWord );
    constructor Create( ImageData : PByte; aSizeX, aSizeY : DWord );
    procedure Fill( Color : TColor );
    function Clone : TImage;
    function getColor( Index : DWord ) : TColor;
    procedure setColor( Index : DWord; Color : TColor );
    function getColor( X,Y : DWord ) : TColor;
    procedure setColor( X,Y : DWord; Color : TColor );
    function getRGBA( Index : DWord ) : PByte;
    function getRGBA( X,Y : DWord ) : PByte;
    procedure SubstituteColor( ColorFrom, ColorTo : TColor );
    procedure Scale( Value : Single );
    procedure Scale( vR, vG, vB : Single );
    procedure SimpleSaturation( Value : Single );
    procedure Saturation( Amount : Single );
    procedure Saturation( vR, vG, vB : Single );
    procedure Invert;
    procedure Contrast( Amount : Integer );
    procedure Contrast( vR, vG, vB : Integer );
    procedure LinearSaturation( Amount : Single );
    procedure LinearSaturation( vR, vG, vB : Single );
    procedure Saturation( vR, vG, vB : Single; cR, cG, cB : Single );
    procedure ApplyMatrix( Matrix : PSingle );
    destructor Destroy; override;
  private
    FRawX  : DWord;
    FRawY  : DWord;
    FSizeX : DWord;
    FSizeY : DWord;
    FSize  : DWord;
    FData  : PByte;

    procedure Resize( NewSizeX, NewSizeY : DWord );
  public
    property RawX  : DWord read FRawX write FRawX;
    property RawY  : DWord read FRawY write FRawY;
    property SizeX : DWord read FSizeX;
    property SizeY : DWord read FSizeY;
    property Size  : DWord read FSize;
    property Data  : PByte read FData;

    property Color[ Index : DWord ] : TColor read getColor write setColor;
    property ColorXY[ X,Y : DWord ] : TColor read getColor write setColor;
    property RGBA[ Index : DWord ] : PByte read getRGBA;
    property RGBAXY[ X,Y : DWord ] : PByte read getRGBA;
  end;

implementation

uses vmath;

const
  LumiRed   = 0.3086;
  LumiGreen = 0.6094;
  LumiBlue  = 0.0820;

{ TImage }

function ByteClamp( s : Single ) : Byte; inline;
begin
    if s > 255 then Exit(255)
  else if s < 0 then Exit(0);
  Exit( Round( s ) );
end;

constructor TImage.Create( aSizeX, aSizeY: DWord );
begin
  FData := nil;
  Resize( aSizeX, aSizeY );
end;

constructor TImage.Create( ImageData: PByte; aSizeX, aSizeY: DWord );
begin
  Assert( ImageData <> nil );
  FData := nil;
  Resize( aSizeX, aSizeY );
  Move( ImageData^, FData^, FSize * 4 );
end;

procedure TImage.Fill(Color: TColor);
begin
  FillDWord( FData^, FSize, Color.toDWord );
end;


function TImage.Clone: TImage;
begin
  Exit( TImage.Create( FData, FSizeX, FSizeY ) );
end;

function TImage.getColor(Index: DWord): TColor;
begin
  Index *= 4;
  getColor.R := FData[ Index   ];
  getColor.G := FData[ Index+1 ];
  getColor.B := FData[ Index+2 ];
  getColor.A := FData[ Index+3 ];
end;

procedure TImage.setColor(Index: DWord; Color: TColor);
begin
  Index *= 4;
  FData[ Index   ] := Color.R;
  FData[ Index+1 ] := Color.G;
  FData[ Index+2 ] := Color.B;
  FData[ Index+3 ] := Color.A;
end;

function TImage.getColor(X, Y: DWord): TColor;
var Index : DWord;
begin
  Index := ( FSizeX * Y + X ) * 4;
  getColor.R := FData[ Index   ];
  getColor.G := FData[ Index+1 ];
  getColor.B := FData[ Index+2 ];
  getColor.A := FData[ Index+3 ];
end;

procedure TImage.setColor(X, Y: DWord; Color: TColor);
var Index : DWord;
begin
  Index := ( FSizeX * Y + X) * 4;
  FData[ Index   ] := Color.R;
  FData[ Index+1 ] := Color.G;
  FData[ Index+2 ] := Color.B;
  FData[ Index+3 ] := Color.A;
end;

function TImage.getRGBA(Index: DWord): PByte;
begin
  Exit( FData + Index * 4 );
end;

function TImage.getRGBA(X, Y: DWord): PByte;
begin
  Exit( FData + ( FSizeX * Y + X ) * 4 );
end;

procedure TImage.SubstituteColor(ColorFrom, ColorTo: TColor);
var cfrom, cto : DWord;
    dwdata     : PDWord;
    count      : DWord;
begin
  cfrom  := ColorFrom.toDWord;
  cto    := ColorTo.toDWord;
  dwdata := PDWord( FData );
  for count := 0 to FSize-1 do
    if dwdata[ count ] = cfrom then
      dwdata[ count ] := cto;
end;

procedure TImage.Scale(Value: Single);
begin
  Scale( Value, Value, Value );
end;

procedure TImage.Scale(vR, vG, vB: Single);
var bdata      : PByte;
    count      : DWord;
begin
  for count := 0 to FSize-1 do
  begin
    bdata := FData + count * 4;
    bdata[0] := ByteClamp( bdata[0] * vR );
    bdata[1] := ByteClamp( bdata[1] * vG );
    bdata[2] := ByteClamp( bdata[2] * vB );
  end;
end;

procedure TImage.SimpleSaturation(Value: Single);
var bdata      : PByte;
    count      : DWord;
    r,g,b,v    : Byte;
    NegValue   : Single;
begin
  NegValue := 1 - Value;
  for count := 0 to FSize-1 do
  begin
    bdata := FData + count * 4;
    r := bdata[0];
    g := bdata[1];
    b := bdata[2];
    v := ( r + g + b - Min( r, g, b ) ) div 2;
    bdata[0] := ByteClamp( r*Value + v*NegValue );
    bdata[1] := ByteClamp( g*Value + v*NegValue );
    bdata[2] := ByteClamp( b*Value + v*NegValue );
  end;
end;

procedure TImage.Saturation( Amount: Single );
begin
  Saturation( Amount, Amount, Amount, LumiRed, LumiGreen, LumiBlue );
end;

procedure TImage.Saturation(vR, vG, vB: Single);
begin
  Saturation( vR, vG, vB, LumiRed, LumiGreen, LumiBlue );
end;

procedure TImage.Invert;
var bdata    : PByte;
    count    : DWord;
begin
  for count := 0 to FSize-1 do
  begin
    bdata := FData + count * 4;
    bdata[0] := 255-bdata[0];
    bdata[1] := 255-bdata[1];
    bdata[2] := 255-bdata[2];
  end;
end;

procedure TImage.Contrast(Amount: Integer);
begin
  Contrast( Amount, Amount, Amount );
end;

procedure TImage.Contrast(vR, vG, vB: Integer);
var bdata    : PByte;
    count    : DWord;
    r,g,b    : Integer;
    fr,fg,fb : Single;
begin
  fr := (259 * (vR + 255)) / (255 * (259 - vR));
  fg := (259 * (vG + 255)) / (255 * (259 - vG));
  fb := (259 * (vB + 255)) / (255 * (259 - vB));
  for count := 0 to FSize-1 do
  begin
    bdata := FData + count * 4;
    r := bdata[0];
    g := bdata[1];
    b := bdata[2];
    bdata[0] := ByteClamp( fr * ( r - 128 ) + 128 );
    bdata[1] := ByteClamp( fg * ( g - 128 ) + 128 );
    bdata[2] := ByteClamp( fb * ( b - 128 ) + 128 );
  end;
end;

procedure TImage.LinearSaturation(Amount: Single);
begin
  Saturation( Amount, Amount, Amount, 0.3333, 0.3334, 0.3333 );
end;

procedure TImage.LinearSaturation(vR, vG, vB: Single);
begin
  Saturation( vR, vG, vB, 0.3333, 0.3334, 0.3333 );
end;

procedure TImage.ApplyMatrix( Matrix: PSingle );
var bdata      : PByte;
    count      : DWord;
    r,g,b      : Byte;
begin
  for count := 0 to FSize-1 do
  begin
    bdata := FData + count * 4;
    r := bdata[0];
    g := bdata[1];
    b := bdata[2];
    bdata[0] := ByteClamp( r*Matrix[0] + g*Matrix[1] + b*Matrix[2]  + 255*Matrix[3] );
    bdata[1] := ByteClamp( r*Matrix[4] + g*Matrix[5] + b*Matrix[6]  + 255*Matrix[7] );
    bdata[2] := ByteClamp( r*Matrix[8] + g*Matrix[9] + b*Matrix[10] + 255*Matrix[11] );
  end;
end;

destructor TImage.Destroy;
begin
  FreeMem( FData );
end;

procedure TImage.Resize( NewSizeX, NewSizeY: DWord );
begin
  if FData <> nil then FreeMem( FData );
  FSizeX := NewSizeX;
  FSizeY := NewSizeY;
  FSize  := NewSizeX * NewSizeY;
  GetMem( FData, FSize * 4 );
end;

procedure TImage.Saturation(vR, vG, vB: Single; cR, cG, cB: Single);
var Matrix : array[0..11] of Single;
    R,G,B  : Single;
begin
  R := (1.0 - vR ) * cR;
  G := (1.0 - vG ) * cG;
  B := (1.0 - vB ) * cB;
  Matrix[0] := R + vR; Matrix[4] := R;      Matrix[8] := R;
  Matrix[1] := G;      Matrix[5] := G + vG; Matrix[9] := G;
  Matrix[2] := B;      Matrix[6] := B;      Matrix[10]:= B+vB;
  Matrix[3] := 0;      Matrix[7] := 0;      Matrix[11]:= 0;
  ApplyMatrix( @Matrix );
end;

end.

