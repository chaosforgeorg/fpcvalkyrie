{$INCLUDE valkyrie.inc}
unit vbitmapfont;
interface

uses Classes, SysUtils, vtextures, vgltypes, vimage, vnode;

type TBitmapFontException = class( Exception );

type TBitmapFontGylph = record
  Present     : Boolean;
  Position    : TGLVec2i;
  Size        : TGLVec2i;
  GLQTexCoord : TGLRawQTexCoord;
end;

type TBitmapFont = class(TVObject)
  constructor CreateFromGrid( aTextureID : TTextureID; aGridX : DWord; aCount : Byte; aShift : Byte = 0 );
  constructor CreateFromGrid( aTextureID : TTextureID; aCount : DWord; aWidthData : PByte; aShift : Byte = 0; aSkip : Byte = 0 );
  destructor Destroy; override;
  procedure SetTexCoord( out aTexCoord : TGLRawQTexCoord; aChar : Char );
  function GetPosition( aChar : Char ) : TGLVec2i;
  function GetSize( aChar : Char ) : TGLVec2i;
protected
  procedure Prepare( aCount : DWord );
  procedure GenerateGridCoord( out aTexCoord : TGLRawQTexCoord; aGylphSize : TGLVec2f; aGridX : Byte; aIndex : Byte );
  function GetGLTexture : DWord;
  function GetImage : TImage;
public
  FGLGylphSize   : TGLVec2f;
  FGylphSize     : TGLVec2i;
  FCount         : DWord;
  FTextureID     : TTextureID;
  FGylphs        : array of TBitmapFontGylph;
public
  property Image       : TImage     read GetImage;
  property GLGylphSize : TGLVec2f   read FGLGylphSize;
  property GylphSize   : TGLVec2i   read FGylphSize;
  property TextureID   : TTextureID read FTextureID;
  property GLTexture   : DWord      read GetGLTexture;
end;

implementation

uses math;

{ TBitmapFont }

constructor TBitmapFont.CreateFromGrid ( aTextureID : TTextureID; aGridX : DWord; aCount : Byte; aShift : Byte ) ;
var iIndex     : Byte;
    iGridY     : Byte;
    iTexture   : TTexture;
begin
  FTextureID := aTextureID;
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
    Size    := FGylphSize;
  end;
end;

constructor TBitmapFont.CreateFromGrid ( aTextureID : TTextureID; aCount : DWord; aWidthData : System.PByte; aShift : Byte; aSkip : Byte ) ;
var iIndex     : Byte;
    iTexture   : TTexture;
begin
  FTextureID := aTextureID;
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
    Size     := FGylphSize;
  end;
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

procedure TBitmapFont.SetTexCoord( out aTexCoord : TGLRawQTexCoord; aChar : Char );
var iIndex : Byte;
begin
  iIndex := Ord( aChar );
  if (iIndex >= FCount) or (not FGylphs[ iIndex ].Present) then
  begin
    // log?
    Exit;
  end;
  aTexCoord := FGylphs[ iIndex ].GLQTexCoord;
end;

function TBitmapFont.GetPosition ( aChar : Char ) : TGLVec2i;
var iIndex : Byte;
begin
  iIndex := Ord( aChar );
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
  if (iIndex >= FCount) or (not FGylphs[ iIndex ].Present) then
  begin
    Exit(TGLVec2i.Create(0,0));
  end;
  Exit( FGylphs[ iIndex ].Size );
end;

end.

