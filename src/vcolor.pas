{$include valkyrie.inc}
unit vcolor;
interface

uses vvector;

type

{ TColor }

TColor = packed object
  R : Byte;
  G : Byte;
  B : Byte;
  A : Byte;
  procedure Init( aR, aG, Ab : Byte; aA : Byte = 255 );
  function toDWord : DWord;
  function toIOColor : DWord;
end;

function ColorMix( A, B : TColor ) : TColor;
function NewColor( aR, aG, Ab : Byte; aA : Byte = 255 ) : TColor;
function NewColor( Color16 : Byte ) : TColor;
function NewColor( aVec : TVec3b ) : TColor;
function NewColor( aVec : TVec4b ) : TColor;
function NewColor( aVec : TVec3f ) : TColor;
function NewColor( aVec : TVec4f ) : TColor;
function ScaleColor( A : TColor; Scale : Byte ) : TColor;
function ScaleColor( A : TColor; Scale : Single ) : TColor;
function ColorLerp( A, B : TColor; Value : Single ) : TColor;

const
  ColorZero      : TColor = ( r : 0;   g : 0;   b : 0;   a : 0; );
  ColorBlack     : TColor = ( r : 0;   g : 0;   b : 0;   a : 255; );
  ColorRed       : TColor = ( r : 255; g : 0;   b : 0;   a : 255; );
  ColorGreen     : TColor = ( r : 0;   g : 255; b : 0;   a : 255; );
  ColorBlue      : TColor = ( r : 0;   g : 0;   b : 255; a : 255; );
  ColorWhite     : TColor = ( r : 255; g : 255; b : 255; a : 255; );

implementation

uses math;

const StandardColors : array[0..15] of array[0..2] of Byte = (
      ( 0,   0,   0 ),
      ( 0,   0,   160 ),
      ( 0,   160, 0 ),
      ( 0,   160, 160 ),
      ( 160, 0,   0 ),
      ( 160, 0,   160 ),
      ( 160, 160, 0 ),
      ( 200, 200, 200 ),
      ( 128, 128, 128 ),
      ( 0,   0,   255 ),
      ( 0,   255, 0 ),
      ( 0,   255, 255 ),
      ( 255, 0,   0 ),
      ( 255, 0,   255 ),
      ( 255, 255, 0 ),
      ( 255, 255, 255 )
      );

function ColorMix(A, B: TColor): TColor;
begin
  ColorMix.R := Round(A.R / 255.0 * B.R );
  ColorMix.G := Round(A.G / 255.0 * B.G );
  ColorMix.B := Round(A.B / 255.0 * B.B );
  ColorMix.A := 255;
end;

function NewColor(aR, aG, Ab: Byte; aA: Byte): TColor;
begin
  NewColor.R := aR;
  NewColor.G := aG;
  NewColor.B := aB;
  NewColor.A := aA;
end;

function NewColor(Color16: Byte): TColor;
begin
  NewColor.R := StandardColors[Color16][0];
  NewColor.G := StandardColors[Color16][1];
  NewColor.B := StandardColors[Color16][2];
  NewColor.A := 255;
end;

function NewColor ( aVec : TVec3b ) : TColor;
begin
  NewColor.R := aVec.X;
  NewColor.G := aVec.Y;
  NewColor.B := aVec.Z;
  NewColor.A := 255;
end;

function NewColor ( aVec : TVec4b ) : TColor;
begin
  NewColor.R := aVec.X;
  NewColor.G := aVec.Y;
  NewColor.B := aVec.Z;
  NewColor.A := aVec.W;
end;

function NewColor ( aVec : TVec3f ) : TColor;
begin
  NewColor.R := Round(aVec.X * 255);
  NewColor.G := Round(aVec.Y * 255);
  NewColor.B := Round(aVec.Z * 255);
  NewColor.A := 255;
end;

function NewColor ( aVec : TVec4f ) : TColor;
begin
  NewColor.R := Round(aVec.X * 255);
  NewColor.G := Round(aVec.Y * 255);
  NewColor.B := Round(aVec.Z * 255);
  NewColor.A := Round(aVec.W * 255);
end;

function ScaleColor(A: TColor; Scale : Byte ): TColor;
begin
  ScaleColor.R := Round(A.R / 255.0 * Scale );
  ScaleColor.G := Round(A.G / 255.0 * Scale );
  ScaleColor.B := Round(A.B / 255.0 * Scale );
  ScaleColor.A := 255;
end;

function ScaleColor(A: TColor; Scale: Single): TColor;
begin
  ScaleColor.R := Min( Round(A.R * Scale ), 255 );
  ScaleColor.G := Min( Round(A.G * Scale ), 255 );
  ScaleColor.B := Min( Round(A.B * Scale ), 255 );
  ScaleColor.A := 255;
end;

function ColorLerp ( A, B : TColor; Value : Single ) : TColor;
begin
  ColorLerp.R := A.R + Round((B.R - A.R ) * Value);
  ColorLerp.G := A.G + Round((B.G - A.G ) * Value);
  ColorLerp.B := A.B + Round((B.B - A.B ) * Value);
  ColorLerp.A := A.A + Round((B.A - A.A ) * Value);
end;

{ TColor }

procedure TColor.Init(aR, aG, Ab: Byte; aA: Byte);
begin
  R := aR;
  G := aG;
  B := aB;
  A := aA;
end;

function TColor.toDWord: DWord;
begin
   toDWord := PDWord(@Self)^;
end;

function TColor.toIOColor : DWord;
begin
   toIOColor := A + B shl 8 + G shl 16 + R shl 24;
end;

end.

