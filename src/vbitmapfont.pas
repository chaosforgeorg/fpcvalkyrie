{$INCLUDE valkyrie.inc}
unit vbitmapfont;
interface

uses Classes, SysUtils, vtextures, vgltypes, vimage, vnode, DOM;

type TBitmapFontGylph = record
  Present     : Boolean;
  Advance     : Word;
  Position    : TGLVec2i;
  Size        : TGLVec2i;
  GLQTexCoord : TGLRawQTexCoord;
  Kerning     : array[32..127] of Byte;
end;

type TBitmapFont = class(TVObject)
  constructor CreateFromGrid( aTextureID : TTextureID; aGridX : DWord; aCount : Byte; aShift : Byte = 0 );
  constructor CreateFromGrid( aTextureID : TTextureID; aCount : DWord; aWidthData : PByte; aShift : Byte = 0; aSkip : Byte = 0 );
  constructor CreateFromXML( aTextureID : TTextureID; aXML : TXMLDocument );
  constructor CreateFromTTF( aTTFStream : TStream; aStreamSize, aSize : Word );
  destructor Destroy; override;
  function SetTexCoord( out aTexCoord : TGLRawQTexCoord; aChar : Char ) : Word;
  function GetPosition( aChar : Char ) : TGLVec2i;
  function GetSize( aChar : Char ) : TGLVec2i;
  function GetAdvance( aChar : Char ) : Word;
  function GetKerning( aChar,aChar2 : Char ) : Word;
protected
  procedure Prepare( aCount : DWord );
  procedure GenerateGridCoord( out aTexCoord : TGLRawQTexCoord; aGylphSize : TGLVec2f; aGridX : Byte; aIndex : Byte );
  function GetGLTexture : DWord;
  function GetImage : TImage;
public
  FGLGylphSize   : TGLVec2f;
  FGylphSize     : TGLVec2i;
  FLowerCase     : Boolean;
  FCount         : DWord;
  FTextureID     : TTextureID;
  FHeight        : Word;
  FAscent        : Integer;
  FDescent       : Integer;
  FLineSkip      : Integer;
  FGylphs        : array of TBitmapFontGylph;
public
  property Image       : TImage     read GetImage;
  property GLGylphSize : TGLVec2f   read FGLGylphSize;
  property GylphSize   : TGLVec2i   read FGylphSize;
  property TextureID   : TTextureID read FTextureID;
  property GLTexture   : DWord      read GetGLTexture;
  property LowerCase   : Boolean    read FLowerCase write FLowerCase;
  property Height      : Word       read FHeight;
  property Ascent      : Integer    read FAscent;
  property Descent     : Integer    read FDescent;
  property LineSkip    : Integer    read FLineSkip;
end;

implementation

uses vsdllibrary, vsdlttflibrary, vmath, math;

function CharNameToChar( const aCharName : AnsiString ) : Char;
begin
  if Length( aCharName ) = 1 then Exit( aCharName[1] );
	if aCharName = 'zero'        then Exit('0');
	if aCharName = 'one'         then Exit('1');
	if aCharName = 'two'         then Exit('2');
	if aCharName = 'three'       then Exit('3');
	if aCharName = 'four'        then Exit('4');
	if aCharName = 'five'        then Exit('5');
	if aCharName = 'six'         then Exit('6');
	if aCharName = 'seven'       then Exit('7');
	if aCharName = 'eight'       then Exit('8');
	if aCharName = 'nine'        then Exit('9');
	if aCharName = 'backslash'   then Exit('\');
	if aCharName = 'frontslash'  then Exit('/');
	if aCharName = 'percent'     then Exit('%');
	if aCharName = 'exclamation' then Exit('!');
	if aCharName = 'question'    then Exit('?');
	if aCharName = 'space'       then Exit(' ');

	if aCharName = 'para_left'       then Exit('(');
	if aCharName = 'para_right'      then Exit(')');
	if aCharName = 'bracket_left'    then Exit('[');
	if aCharName = 'bracket_right'   then Exit(']');
	if aCharName = 'guillemet_left'  then Exit('<');
	if aCharName = 'guillemet_right' then Exit('>');

	if aCharName = 'period'      then Exit('.');
	if aCharName = 'comma'       then Exit(',');
	if aCharName = 'colon'       then Exit(':');
	if aCharName = 'semicolon'   then Exit(';');
	if aCharName = 'at'          then Exit('@');
	if aCharName = 'quote'       then Exit('"');
	if aCharName = 'singlequote' then Exit('''');
	if aCharName = 'caret'       then Exit('^');
	if aCharName = 'amp'         then Exit('&');
	if aCharName = 'dollar'      then Exit('$');
	if aCharName = 'euro'        then Exit(#0);
	if aCharName = 'number'      then Exit('#');
	if aCharName = 'bullet'      then Exit(#0);
	if aCharName = 'vbar'        then Exit('|');
	if aCharName = 'dash'        then Exit(#0);
	if aCharName = 'minus'       then Exit('-');
	if aCharName = 'plus'        then Exit('+');
	if aCharName = 'equals'      then Exit('=');
	if aCharName = 'cross'       then Exit('*');
  Exit(#0);
end;

{ TBitmapFont }

constructor TBitmapFont.CreateFromGrid ( aTextureID : TTextureID; aGridX : DWord; aCount : Byte; aShift : Byte ) ;
var iIndex     : Byte;
    iCount     : Byte;
    iGridY     : Byte;
    iTexture   : TTexture;
begin
  FTextureID := aTextureID;
  FLowerCase := False;
  Prepare( aCount + aShift );
  iGridY     := Ceil( aCount / aGridX );
  iTexture   := TTextureManager.Get().Texture[ aTextureID ];
  FGylphSize.Init( iTexture.Size.X div aGridX, iTexture.Size.Y div iGridY );
  FGLGylphSize.Init( iTexture.GLSize.X / aGridX, iTexture.GLSize.Y / iGridY );

  for iIndex := 0 to aCount - 1 do
  with FGylphs[iIndex+aShift] do
  begin
    GenerateGridCoord( GLQTexCoord, FGLGylphSize, aGridX, iIndex );
    Position.Init( 0, 0 );
    Present := True;
    Advance := FGylphSize.X + 1;
    Size    := FGylphSize;
    for iCount := Low( Kerning ) to High( Kerning ) do
      Kerning[ iCount ] := Advance;
  end;

  FHeight        := FGylphSize.Y;
  FAscent        := FGylphSize.Y;
  FDescent       := 0;
  FLineSkip      := FHeight+1;
end;

constructor TBitmapFont.CreateFromGrid ( aTextureID : TTextureID; aCount : DWord; aWidthData : System.PByte; aShift : Byte; aSkip : Byte ) ;
var iIndex     : Byte;
    iCount     : Byte;
    iTexture   : TTexture;
begin
  FTextureID := aTextureID;
  FLowerCase := False;
  Prepare( aCount + aShift );
  iTexture   := TTextureManager.Get().Texture[ aTextureID ];
  FGylphSize.Init( iTexture.Size.X div 16, iTexture.Size.Y div 16 );
  FGLGylphSize.Init( iTexture.GLSize.X / 16, iTexture.GLSize.Y / 16 );

  for iIndex := 0 to aCount - 1 do
  with FGylphs[iIndex+aShift] do
  begin
    GenerateGridCoord( GLQTexCoord, FGLGylphSize, 16, iIndex+aSkip );
    Position.Init( 0, 0 );
    Present  := True;
    Advance  := aWidthData[ iIndex ] + 1;
    Size     := FGylphSize;
    for iCount := Low( Kerning ) to High( Kerning ) do
      Kerning[ iCount ] := Advance;
  end;

  FHeight        := FGylphSize.Y;
  FAscent        := FGylphSize.Y;
  FDescent       := 0;
  FLineSkip      := FHeight+1;
end;

constructor TBitmapFont.CreateFromXML ( aTextureID : TTextureID; aXML : TXMLDocument ) ;
var iCount         : Word;
    iCount2        : Word;
    iCharElement   : TDOMElement;
    iChar          : Char;
    iTexture       : TTexture;
    iPixel         : TGLVec2f;
    iPosA, iPosB   : TGLVec2i;
    iTPosA, iTPosB : TGLVec2f;
begin
  FTextureID := aTextureID;
  FLowerCase := True;
  Prepare( 128 );
  iTexture   := TTextureManager.Get().Texture[ aTextureID ];
  FGylphSize.Init( 0,0 );
  iPixel.Init( iTexture.GLSize.X / iTexture.Size.X, iTexture.GLSize.Y / iTexture.Size.Y );

  if aXML.DocumentElement.ChildNodes.Count > 0 then
  for iCount := 0 to aXML.DocumentElement.ChildNodes.Count-1 do
  begin
    iCharElement := TDOMElement( aXML.DocumentElement.ChildNodes.Item[iCount] );
    iChar        := CharNameToChar( iCharElement.GetAttribute('name') );
    if (iChar = #0) or (Ord(iChar) > 127) then Continue;
    if iChar in ['A'..'Z'] then FLowerCase := False;
    iPosA.X := StrToInt( iCharElement.GetElementsByTagName('pos_x')[0].TextContent );
    iPosA.Y := StrToInt( iCharElement.GetElementsByTagName('pos_y')[0].TextContent );
    iPosB.X := StrToInt( iCharElement.GetElementsByTagName('width')[0].TextContent );
    iPosB.Y := StrToInt( iCharElement.GetElementsByTagName('height')[0].TextContent );
    FGylphSize.X := Max( FGylphSize.X, iPosB.X );
    FGylphSize.Y := Max( FGylphSize.Y, iPosB.Y );

    FGylphs[ Ord(iChar) ].Present := True;
    FGylphs[ Ord(iChar) ].Advance := iPosB.X + 1;
    FGylphs[ Ord(iChar) ].Size    := iPosB;
    iTPosA.Init( iPixel.X * iPosA.X, iPixel.Y * iPosA.Y );
    iTPosB.Init( iPixel.X * iPosB.X, iPixel.Y * iPosB.Y );
    FGylphs[ Ord(iChar) ].GLQTexCoord.Init( iTPosA, iTPosA + iTPosB );

    for iCount2 := Low( FGylphs[ Ord(iChar) ].Kerning ) to High( FGylphs[ Ord(iChar) ].Kerning ) do
      FGylphs[ Ord(iChar) ].Kerning[ iCount2 ] := FGylphs[ Ord(iChar) ].Advance ;
  end;
  FGLGylphSize.Init( iPixel.X * FGylphSize.X, iPixel.Y * FGylphSize.Y );

  for iCount := 0 to 127 do
    if FGylphs[ iCount ].Present then
      FGylphs[ iCount ].Position.Init( 0, 0 );


  FHeight        := FGylphSize.Y;
  FAscent        := FGylphSize.Y;
  FDescent       := 0;
  FLineSkip      := FHeight+1;
end;

constructor TBitmapFont.CreateFromTTF ( aTTFStream : TStream; aStreamSize, aSize : Word ) ;
var iFont     : PTTF_Font;
    iMap      : PSDL_Surface;
    iColor    : TSDL_Color;
    iCache    : array[32..127] of PSDL_Surface;
    iMaxLen   : array[0..5] of Integer;
    iCount    : DWord;
    iCount2   : DWord;
    iTexWidth : LongInt;
    iImage    : TImage;
    iID       : AnsiString;

    iChar3    : array[0..2] of Char;
    iWidth    : Integer;
    iUnused   : Integer;
    iGMMinX   : Integer;
    iGMMaxX   : Integer;
    iGMMinY   : Integer;
    iGMMaxY   : Integer;
    iGMAdv    : Integer;
    iX, iY    : Integer;
    iRect     : SDL_Rect;
    iPixel    : TGLVec2f;
    iPosA     : TGLVec2f;
    iPosB     : TGLVec2f;
begin
  LoadSDLTTF;
  if TTF_WasInit() = 0 then TTF_Init();

  FLowerCase := False;
  Prepare( 128 );

  iMap  := nil;
  iFont := TTF_OpenFontRWOrThrow( SDL_RWopsFromStream( aTTFStream, aStreamSize ), 0, aSize );

  FHeight   := TTF_FontHeight( iFont );
  FAscent   := TTF_FontAscent( iFont );
  FDescent  := TTF_FontDescent( iFont );
  FLineSkip := TTF_FontLineSkip( iFont );
  iID       := TTF_FontFaceFamilyName( iFont );

  for iCount := 0 to 5 do iMaxLen[iCount] := 0;
  iColor.r := 255;
  iColor.g := 255;
  iColor.b := 255;
  iColor.unused := 0;

  for iCount := 32 to 127 do
  begin
    iCache[iCount] := TTF_RenderGlyph_Blended( iFont, iCount, iColor );
    SDL_SetAlpha( iCache[iCount], 0, 0 );
    iMaxLen[ (iCount-32) div 16 ] += iCache[iCount]^.w;
  end;

  iTexWidth := 0;
  for iCount := 0 to 5 do
    if iMaxLen[iCount] > iTexWidth then
      iTexWidth := iMaxLen[iCount];

  iTexWidth := UpToPowerOf2( iTexWidth );

  iMap := SDL_CreateRGBSurface( SDL_SWSURFACE, iTexWidth, iTexWidth, 32,
{$IFDEF ENDIAN_LITTLE}
            $000000FF, $0000FF00, $00FF0000, $FF000000
{$ELSE}
            $FF000000, $00FF0000, $0000FF00, $000000FF
{$ENDIF}
  );

  SDL_FillRect( iMap, nil, SDL_MapRGBA( iMap^.format, 0, 0, 0, 0) );

  iPixel.Init( 1.0 / iTexWidth, 1.0 / iTexWidth );
  iX := 0;
  iY := 0;

  for iCount := 32 to 127 do
  begin
    FGylphs[iCount].Present := True;
    TTF_GlyphMetrics( iFont, iCount, iGMMinX, iGMMaxX,  iGMMinY, iGMMaxY, iGMAdv );
    iRect.x := iX;
    iRect.y := iY;
    iRect.w := iCache[iCount]^.w;
    iRect.h := iCache[iCount]^.h;

    SDL_UpperBlit( iCache[iCount], nil, iMap, @iRect );
    FGylphs[iCount].Advance := iGMAdv;
    FGylphs[iCount].Position.Init( iGMMinX, FAscent - iGMMaxY );
    FGylphs[iCount].Size.Init(iCache[iCount]^.w,iCache[iCount]^.h);
    iPosA.Init( iX * iPixel.X, iY * iPixel.Y );
    iPosB.Init( (iX + iCache[iCount]^.w) * iPixel.X, (iY + iCache[iCount]^.h) * iPixel.Y );
    FGylphs[iCount].GLQTexCoord.Init(iPosA,iPosB);
    iX += iCache[iCount]^.w;
    if iCount mod 16 = 15 then
    begin
		  iY += FHeight;
		  iX := 0;
    end;
    for iCount2 := 32 to 127 do
    begin
      iChar3[0] := Chr(iCount2);
      iChar3[1] := Chr(iCount);
      iChar3[2] := #0;
      TTF_SizeText( iFont, iChar3, iWidth, iUnused );
      FGylphs[iCount2].Kerning[iCount] := iWidth - iGMAdv;
    end;
  end;
  for iCount := 32 to 127 do
  begin

    SDL_FreeSurface( iCache[ iCount ] );
  end;

  TTF_CloseFont( iFont );

  SDL_SetAlpha( iMap,SDL_SRCALPHA or SDL_RLEACCEL , 0 );
//  SDL_SetAlpha( iMap, 0, 0 );
  SDL_SaveBMP( iMap, 'test.bmp' );

  iImage := TImage.Create( iMap^.pixels, iTexWidth, iTexWidth );
  for iCount := 512 to 2000 do Log( IntToStr( iImage.Color[ iCount ].A ) );
  iImage.RawX := iTexWidth;
  iImage.RawY := iTexWidth;
  SDL_FreeSurface( iMap );

  iID += '_'+IntToStr( aSize );
  FTextureID := TTextureManager.Get().AddImage( iID, iImage, False );
  TTextureManager.Get()[FTextureID].Upload;

  FGylphSize.Init( iGMMaxX - iGMMinX, iGMMaxY - iGMMinY );
  FGLGylphSize.Init( iPixel.X * FGylphSize.X, iPixel.Y * FGylphSize.Y );
end;

procedure TBitmapFont.Prepare ( aCount : DWord ) ;
var iCount : DWord;
begin
  SetLength( FGylphs, aCount );
  FCount  := aCount;
  for iCount := 0 to aCount-1 do
    FGylphs[ iCount ].Present := false;
end;

procedure TBitmapFont.GenerateGridCoord ( out aTexCoord : TGLRawQTexCoord; aGylphSize : TGLVec2f; aGridX : Byte; aIndex : Byte ) ;
var iPosA, iPosB : TGLVec2f;
    iTex         : TGLVec2i;
begin
  iTex  := TGLVec2i.CreateModDiv( aIndex, aGridX );
  iPosA := TGLVec2f.Create( iTex.X * aGylphSize.X, iTex.Y * aGylphSize.Y );
  iPosB := TGLVec2f.Create( (iTex.X+1) * aGylphSize.X, (iTex.Y+1) * aGylphSize.Y );
  aTexCoord.Init( iPosA, iPosB );
end;

function TBitmapFont.GetGLTexture : DWord;
begin
  Exit( TTextureManager.Get().Texture[ FTextureID ].GLTexture )
end;

function TBitmapFont.GetImage : TImage;
begin
  Exit( TTextureManager.Get().Texture[ FTextureID ].Image )
end;

destructor TBitmapFont.Destroy;
begin
  inherited Destroy;
end;

function TBitmapFont.SetTexCoord( out aTexCoord : TGLRawQTexCoord; aChar : Char ) : Word;
var iIndex : Byte;
begin
  iIndex := Ord( aChar );
  if FLowerCase and (iIndex in [Ord('A')..Ord('Z')]) then iIndex := iIndex + 32;
  if (iIndex >= FCount) or (not FGylphs[ iIndex ].Present) then
  begin
    // log?
    Exit(0);
  end;
  aTexCoord := FGylphs[ iIndex ].GLQTexCoord;
  Exit( FGylphs[ iIndex ].Advance );
end;

function TBitmapFont.GetPosition ( aChar : Char ) : TGLVec2i;
var iIndex : Byte;
begin
  iIndex := Ord( aChar );
  if FLowerCase and (iIndex in [Ord('A')..Ord('Z')]) then iIndex := iIndex + 32;
  if (iIndex >= FCount) or (not FGylphs[ iIndex ].Present) then
  begin
    Exit(TGLVec2i.Create(0,0));
  end;
  Exit( FGylphs[ iIndex ].Position );
end;

function TBitmapFont.GetSize ( aChar : Char ) : TGLVec2i;
var iIndex : Byte;
begin
  iIndex := Ord( aChar );
  if FLowerCase and (iIndex in [Ord('A')..Ord('Z')]) then iIndex := iIndex + 32;
  if (iIndex >= FCount) or (not FGylphs[ iIndex ].Present) then
  begin
    Exit(TGLVec2i.Create(0,0));
  end;
  Exit( FGylphs[ iIndex ].Size );
end;

function TBitmapFont.GetAdvance ( aChar : Char ) : Word;
var iIndex : Byte;
begin
  iIndex := Ord( aChar );
  if FLowerCase and (iIndex in [Ord('A')..Ord('Z')]) then iIndex := iIndex + 32;
  if (iIndex >= FCount) or (not FGylphs[ iIndex ].Present) then
  begin
    Exit(0);
  end;
  Exit( FGylphs[ iIndex ].Advance );
end;

function TBitmapFont.GetKerning ( aChar, aChar2 : Char ) : Word;
var iIndex  : Byte;
    iIndex2 : Byte;
begin
  iIndex  := Ord( aChar );
  iIndex2 := Ord( aChar2 );
  if FLowerCase then
  begin
    if (iIndex  in [Ord('A')..Ord('Z')]) then iIndex  := iIndex  + 32;
    if (iIndex2 in [Ord('A')..Ord('Z')]) then iIndex2 := iIndex2 + 32;
  end;
  if (iIndex  >= FCount) or (not FGylphs[ iIndex  ].Present)
  or (iIndex2 >= FCount) or (not FGylphs[ iIndex2 ].Present) then
  begin
    Exit(0);
  end;
  Exit( FGylphs[ iIndex ].Kerning[ iIndex2 ] );
end;


end.

