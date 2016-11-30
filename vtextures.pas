{$include valkyrie.inc}
unit vtextures;
interface
uses Classes, SysUtils, vnode, vimage, vgenerics, vgltypes;

type TTexture = class( TVObject )
  constructor Create( aImage : TImage; aBlend : Boolean );
  procedure Upload;
  destructor Destroy; override;
private
  FImage  : TImage;
  FBlend  : Boolean;
  FGLID   : DWord;
  FSize   : TGLVec2i;
  FGLSize : TGLVec2f;
public
  property Image     : TImage   read FImage;
  property GLSize    : TGLVec2f read FGLSize;
  property Size      : TGLVec2i read FSize;
  property Blend     : Boolean  read FBlend write FBlend;
  property GLTexture : DWord    read FGLID;
end;

type TTextureID        = DWord;
type TTextureIDHashMap = specialize TGHashMap<TTextureID>;
type TTextureArray     = specialize TGObjectArray<TTexture>;


type TTextureManager = class( TVObject )
  constructor Create( aDefaultBlend : Boolean = False );
  class function Get : TTextureManager;
  class function Initialized : Boolean;
  destructor Destroy; override;

  function AddImage( const aID : AnsiString; aImage : TImage; aBlend : Boolean ) : TTextureID;
  function AddFile( const aFilename : AnsiString; aBlend : Boolean ) : TTextureID;
  function AddFile( const aFilename : AnsiString; const aID : AnsiString; aBlend : Boolean ) : TTextureID;
  procedure LoadTextureFolder( const aFolder : AnsiString );
  procedure LoadTextureCallback( aStream : TStream; aName : Ansistring; aSize : DWord );
  procedure Upload;

  function GetTextureID( const aTextureName : AnsiString ) : TTextureID;
  function GetTexture( const aTextureID : TTextureID ) : TTexture;
  function GetTexture( const aTextureName : AnsiString ) : TTexture;
protected
  FTextureIDs : TTextureIDHashMap;
  FTextures   : TTextureArray;
  FBlendDef   : Boolean;
public
  property TextureID[ const aIndex : AnsiString ] : TTextureID read GetTextureID;
  property Texture[ const aID : TTextureID ] : TTexture read GetTexture; default;
  property Textures[ const aIndex : AnsiString ] : TTexture read GetTexture;
  property BlendDefault : Boolean read FBlendDef write FBlendDef;
end;

implementation

uses vgllibrary, vglimage;

var TextureManager : TTextureManager = nil;

constructor TTexture.Create( aImage : TImage; aBlend : Boolean );
begin
  FImage := aImage;
  FBlend := aBlend;
  FGLID  := 0;
  FSize   := TGLVec2i.Create( aImage.RawX, aImage.RawY );
  FGLSize := TGLVec2f.Create( aImage.RawX / aImage.SizeX, aImage.RawY / aImage.SizeY );
end;

procedure TTexture.Upload;
var iGLBlend   : TGLInt;
begin
  glGenTextures( 1, @FGLID );
  if FBlend
    then iGLBlend := GL_LINEAR
    else iGLBlend := GL_NEAREST;

  glBindTexture(GL_TEXTURE_2D, FGLID);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, iGLBlend );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, iGLBlend );
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
    FImage.SizeX, FImage.SizeY, 0, GL_RGBA, GL_UNSIGNED_BYTE, FImage.Data );
end;

destructor TTexture.Destroy;
begin
  glDeleteTextures( 1, @FGLID );
  inherited Destroy;
end;

{ TTextureManager }

constructor TTextureManager.Create( aDefaultBlend : Boolean = False );
begin
  LoadGL;
  Assert( TextureManager = nil );
  TextureManager := Self;
  FTextureIDs := TTextureIDHashMap.Create( HashMap_RaiseAll );
  FTextures   := TTextureArray.Create;
  FBlendDef   := aDefaultBlend;
end;

class function TTextureManager.Get : TTextureManager;
begin
  Assert( TextureManager <> nil );
  Exit( TextureManager );
end;

class function TTextureManager.Initialized : Boolean;
begin
  Exit( TextureManager <> nil );
end;

destructor TTextureManager.Destroy;
begin
  TextureManager := nil;
  FreeAndNil( FTextureIDs );
  FreeAndNil( FTextures );
  inherited Destroy;
end;

function TTextureManager.AddImage ( const aID : AnsiString; aImage : TImage; aBlend : Boolean ) : TTextureID;
begin
  Result := FTextures.Push( TTexture.Create(aImage, aBlend) );
  FTextureIDs[ aID ] := Result;
end;

function TTextureManager.AddFile ( const aFilename : AnsiString; aBlend : Boolean ) : TTextureID;
var iName      : AnsiString;
begin
  iName := aFileName;
  Delete(iName,Length(iName)-3,4);
  Exit( AddImage( iName, LoadImage( aFileName ), aBlend ) );
end;

function TTextureManager.AddFile ( const aFilename : AnsiString; const aID : AnsiString; aBlend : Boolean ) : TTextureID;
begin
  Exit( AddImage( aID, LoadImage( aFileName ), aBlend ) );
end;

procedure TTextureManager.LoadTextureFolder ( const aFolder : AnsiString ) ;
var iName      : AnsiString;
    iSearchRec : TSearchRec;
begin
  if FindFirst(aFolder + PathDelim + '*.png',faAnyFile,iSearchRec) = 0 then
  repeat
    iName := iSearchRec.Name;
    Delete(iName,Length(iName)-3,4);
    AddImage( iName, LoadImage(aFolder + PathDelim + iSearchRec.Name ), FBlendDef );
  until (FindNext(iSearchRec) <> 0);
end;

procedure TTextureManager.LoadTextureCallback ( aStream : TStream; aName : Ansistring; aSize : DWord ) ;
var iName      : AnsiString;
begin
  iName := aName;
  Delete(iName,Length(iName)-3,4);
  AddImage( iName, LoadImage( aStream, aSize ), FBlendDef );
end;

procedure TTextureManager.Upload;
var iIndex : DWord;
begin
  if FTextures.Size = 0 then Exit;
  for iIndex := 0 to FTextures.Size-1 do
    FTextures[ iIndex ].Upload;
end;

function TTextureManager.GetTextureID ( const aTextureName : AnsiString ) : TTextureID;
begin
  Exit( FTextureIDs[ aTextureName ] );
end;

function TTextureManager.GetTexture ( const aTextureID : TTextureID ) : TTexture;
begin
  Exit( FTextures[ aTextureID-1 ] );
end;

function TTextureManager.GetTexture ( const aTextureName : AnsiString ) : TTexture;
begin
  Exit( FTextures[ FTextureIDs[ aTextureName ]-1 ] );
end;



end.

