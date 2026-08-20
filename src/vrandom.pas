{$INCLUDE valkyrie.inc}
// @abstract(Mersenne Twister random number generator for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
//
// The MT19937 implementation is based on mt19937ar.c:
//
// Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. The names of its contributors may not be used to endorse or promote
//    products derived from this software without specific prior written
//    permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

unit vrandom;
interface

uses Classes, SysUtils, vnode;

type

{ TRNG }

TRNG = class( TVObject )
private
  FState : array[0..623] of DWord;
  FIndex : DWord;
  function BoundedDWord( aRange : DWord ) : DWord;
  procedure Twist;
public
  constructor Create( aSeed : DWord ); reintroduce;
  constructor Create; reintroduce;
  constructor CreateFromStream( aStream : TStream ); override;
  procedure WriteToStream( aStream : TStream ); override;
  procedure SetSeed( aSeed : DWord );
  procedure Randomize;
  // Rolls aNumber dice with aSides sides
  function Dice( aNumber, aSides : DWord ) : DWord;
  // Returns every value in 0..2^32-1
  function RDWord : DWord; overload; inline;
  // Returns a value in [0,aRange), or 0 when aRange=0
  function RDWord( aRange : DWord ) : DWord; overload; inline;
  // Returns a value in inclusive [aMin,aMax], or aMin when aMin>=aMax
  function RDWord( aMin, aMax : DWord ) : DWord; overload;
  // Returns a uniformly spaced Single in [0,1)
  function RFloat : Single; overload; inline;
  // Returns a Single in [0,aRange), or 0 when aRange<=0
  function RFloat( aRange : Single ) : Single; overload;
  // Returns a Single in inclusive [aMin,aMax], or aMin when aMin>=aMax
  function RFloat( aMin, aMax : Single ) : Single; overload;
  // Returns a uniformly spaced Double in [0,1)
  function RDouble : Double;
  // Returns every value in 0..2^64-1
  function RQWord : QWord;
  // Returns every value in -2^31..2^31-1
  function RLongInt : LongInt; overload; inline;
  // Returns a value in [0,aRange), or 0 when aRange<=0
  function RLongInt( aRange : LongInt ) : LongInt; overload; inline;
  // Returns a value in inclusive [aMin,aMax], or aMin when aMin>=aMax
  function RLongInt( aMin, aMax : LongInt ) : LongInt; overload;
  // Returns every value in -2^63..2^63-1
  function RInt64 : Int64;
end;

// Process-wide generator, randomized during unit initialization.
var VRNG : TRNG = nil;

implementation

const
  MT_N          = 624;
  MT_M          = 397;
  MT_MATRIX_A   = DWord( $9908B0DF );
  MT_UPPER_MASK = DWord( $80000000 );
  MT_LOWER_MASK = DWord( $7FFFFFFF );

var
  GRandomizeCounter : QWord = 0;

function PreviousSingle( aValue : Single ) : Single;
var iBits : DWord;
begin
  if not ( aValue > 0 ) then Exit( aValue );
  iBits := 0;
  Move( aValue, iBits, SizeOf( iBits ) );
  Dec( iBits );
  Result := 0;
  Move( iBits, Result, SizeOf( Result ) );
end;

constructor TRNG.Create( aSeed : DWord );
begin
  inherited Create;
  SetSeed( aSeed );
end;

constructor TRNG.Create;
begin
  inherited Create;
  Self.Randomize;
end;

constructor TRNG.CreateFromStream( aStream : TStream );
var iIndex : Integer;
begin
  inherited CreateFromStream( aStream );
  FIndex := aStream.ReadDWord;
  if FIndex > MT_N then
    raise EReadError.CreateFmt( 'TRNG stream index out of range: %u', [FIndex] );
  for iIndex := 0 to MT_N - 1 do
    FState[ iIndex ] := aStream.ReadDWord;
end;

procedure TRNG.WriteToStream( aStream : TStream );
var iIndex : Integer;
begin
  inherited WriteToStream( aStream );
  aStream.WriteDWord( FIndex );
  for iIndex := 0 to MT_N - 1 do
    aStream.WriteDWord( FState[ iIndex ] );
end;

{$PUSH}
{$Q-}
{$R-}
procedure TRNG.SetSeed( aSeed : DWord );
var iIndex : DWord;
begin
  FState[0] := aSeed;
  for iIndex := 1 to MT_N - 1 do
    FState[ iIndex ] :=
      DWord( 1812433253 ) *
      ( FState[ iIndex - 1 ] xor ( FState[ iIndex - 1 ] shr 30 ) ) +
      iIndex;
  FIndex := MT_N;
end;
{$POP}

{$PUSH}
{$Q-}
{$R-}
procedure TRNG.Randomize;
var iDateTime : TDateTime;
    iDateBits : QWord;
    iEntropy  : QWord;
begin
  iDateTime := Now;
  iDateBits := 0;
  Move( iDateTime, iDateBits, SizeOf( iDateBits ) );
  Inc( GRandomizeCounter );

  iEntropy := iDateBits xor GetTickCount64 xor
    ( QWord( PtrUInt( Self ) ) shl 1 ) xor
    ( GRandomizeCounter * QWord( $9E3779B97F4A7C15 ) );
  iEntropy := ( iEntropy xor ( iEntropy shr 30 ) ) *
    QWord( $BF58476D1CE4E5B9 );
  iEntropy := ( iEntropy xor ( iEntropy shr 27 ) ) *
    QWord( $94D049BB133111EB );
  iEntropy := iEntropy xor ( iEntropy shr 31 );

  SetSeed( DWord( iEntropy xor ( iEntropy shr 32 ) ) );
end;
{$POP}

function TRNG.Dice( aNumber, aSides : DWord ) : DWord;
var iCount : DWord;
begin
  Result := 0;
  if ( aNumber = 0 ) or ( aSides = 0 ) then Exit;
  if aSides = 1 then Exit( aNumber );
  for iCount := 1 to aNumber do
    Result := Result + RDWord( aSides ) + 1;
end;

procedure TRNG.Twist;
var iIndex : Integer;
    iValue : DWord;
begin
  for iIndex := 0 to MT_N - MT_M - 1 do
  begin
    iValue := ( FState[ iIndex ] and MT_UPPER_MASK ) or
      ( FState[ iIndex + 1 ] and MT_LOWER_MASK );
    FState[ iIndex ] := FState[ iIndex + MT_M ] xor ( iValue shr 1 );
    if ( iValue and 1 ) <> 0 then
      FState[ iIndex ] := FState[ iIndex ] xor MT_MATRIX_A;
  end;

  for iIndex := MT_N - MT_M to MT_N - 2 do
  begin
    iValue := ( FState[ iIndex ] and MT_UPPER_MASK ) or
      ( FState[ iIndex + 1 ] and MT_LOWER_MASK );
    FState[ iIndex ] := FState[ iIndex + MT_M - MT_N ] xor
      ( iValue shr 1 );
    if ( iValue and 1 ) <> 0 then
      FState[ iIndex ] := FState[ iIndex ] xor MT_MATRIX_A;
  end;

  iValue := ( FState[ MT_N - 1 ] and MT_UPPER_MASK ) or
    ( FState[0] and MT_LOWER_MASK );
  FState[ MT_N - 1 ] := FState[ MT_M - 1 ] xor ( iValue shr 1 );
  if ( iValue and 1 ) <> 0 then
    FState[ MT_N - 1 ] := FState[ MT_N - 1 ] xor MT_MATRIX_A;

  FIndex := 0;
end;

function TRNG.RDWord : DWord; inline;
begin
  if FIndex >= Length( FState ) then Twist;

  Result := FState[ FIndex ];
  Inc( FIndex );

  Result := Result xor ( Result shr 11 );
  Result := Result xor ( ( Result shl 7 ) and DWord( $9D2C5680 ) );
  Result := Result xor ( ( Result shl 15 ) and DWord( $EFC60000 ) );
  Result := Result xor ( Result shr 18 );
end;

function TRNG.BoundedDWord( aRange : DWord ) : DWord;
var iCardinality : QWord;
    iLimit       : QWord;
    iValue       : DWord;
begin
  if aRange = 0 then Exit( 0 );

  if aRange <= $10000 then
    Exit( RDWord mod aRange );

  if ( aRange and ( aRange - 1 ) ) = 0 then
    Exit( RDWord and ( aRange - 1 ) );

  iCardinality := QWord( High( DWord ) ) + 1;
  iLimit := iCardinality - ( iCardinality mod QWord( aRange ) );
  repeat
    iValue := RDWord;
  until QWord( iValue ) < iLimit;
  Result := iValue mod aRange;
end;

function TRNG.RDWord( aRange : DWord ) : DWord; inline;
begin
  Result := BoundedDWord( aRange );
end;

function TRNG.RDWord( aMin, aMax : DWord ) : DWord;
var iCardinality : QWord;
    iSpan        : QWord;
begin
  if aMin >= aMax then Exit( aMin );

  iCardinality := QWord( High( DWord ) ) + 1;
  iSpan := QWord( aMax ) - QWord( aMin ) + 1;
  if iSpan = iCardinality then
  begin
    Result := Self.RDWord();
    Exit;
  end;
  Result := aMin + BoundedDWord( DWord( iSpan ) );
end;

function TRNG.RFloat : Single; inline;
begin
  Result := Single( RDWord shr 8 ) * Single( 1.0 / 16777216.0 );
end;

function TRNG.RFloat( aRange : Single ) : Single;
var iUnit : Single;
begin
  if not ( aRange > 0 ) then Exit( 0 );

  iUnit := Self.RFloat();
  if iUnit = 0 then Exit( 0 );
  Result := Single( Double( iUnit ) * Double( aRange ) );
  if not ( Result < aRange ) then
    Result := PreviousSingle( aRange );
end;

function TRNG.RFloat( aMin, aMax : Single ) : Single;
var iValue : DWord;
begin
  if not ( aMax > aMin ) then Exit( aMin );

  iValue := RDWord shr 8;
  if iValue = 0 then Exit( aMin );
  if iValue = $FFFFFF then Exit( aMax );
  Result := Single(
    Double( aMin ) +
    ( Double( aMax ) - Double( aMin ) ) *
    ( Double( iValue ) / 16777215.0 )
  );
  if Result < aMin then Result := aMin;
  if Result > aMax then Result := aMax;
end;

function TRNG.RDouble : Double;
var iHigh : DWord;
    iLow  : DWord;
begin
  iHigh := RDWord shr 5;
  iLow  := RDWord shr 6;
  Result := ( Double( iHigh ) * 67108864.0 + Double( iLow ) ) *
    ( 1.0 / 9007199254740992.0 );
end;

function TRNG.RQWord : QWord;
var iHigh : DWord;
    iLow  : DWord;
begin
  iHigh := RDWord;
  iLow  := RDWord;
  Result := ( QWord( iHigh ) shl 32 ) or QWord( iLow );
end;

function TRNG.RLongInt : LongInt; inline;
var iValue : DWord;
begin
  iValue := RDWord;
  Result := 0;
  Move( iValue, Result, SizeOf( Result ) );
end;

function TRNG.RLongInt( aRange : LongInt ) : LongInt; inline;
begin
  if aRange <= 0 then Exit( 0 );
  Result := LongInt( BoundedDWord( DWord( aRange ) ) );
end;

function TRNG.RLongInt( aMin, aMax : LongInt ) : LongInt;
var iCardinality : QWord;
    iOffset      : DWord;
    iSpan        : QWord;
begin
  if aMin >= aMax then Exit( aMin );

  iCardinality := QWord( High( DWord ) ) + 1;
  iSpan := QWord( Int64( aMax ) - Int64( aMin ) ) + 1;
  if iSpan = iCardinality then
    iOffset := RDWord
  else
    iOffset := BoundedDWord( DWord( iSpan ) );
  Result := LongInt( Int64( aMin ) + Int64( iOffset ) );
end;

function TRNG.RInt64 : Int64;
var iValue : QWord;
begin
  iValue := RQWord;
  Result := 0;
  Move( iValue, Result, SizeOf( Result ) );
end;

initialization
  VRNG := TRNG.Create;

finalization
  FreeAndNil( VRNG );

end.
