unit vglimage;
{$include valkyrie.inc}
interface
uses Classes, SysUtils,
  vimage;

type TImageException = class( Exception );

function LoadImage( const FileName : Ansistring ) : TImage;
function LoadImage( Stream : TStream; Size : DWord ) : TImage;
function UploadImage( Image : TImage; aBlend : Boolean ) : DWord;
procedure ReUploadImage( GLTexture : DWord; Image: TImage; aBlend : Boolean );
procedure UnUploadImage( GLTexture : DWord );

implementation

uses
  vgl3library,
  vsdl3library,
  vsdl3imagelibrary;


function LoadImage( aSurface : PSDL_Surface ): TImage;
var iBufSize : QWord;
    iBuf     : Pointer;
begin
  Assert( aSurface <> nil );
  iBufSize := QWord(aSurface^.w) * QWord(aSurface^.h) * 4;
  GetMem(iBuf, iBufSize);
  if not SDL_ConvertPixels(
    aSurface^.w, aSurface^.h,
    aSurface^.format, aSurface^.pixels, aSurface^.pitch,
    SDL_PIXELFORMAT_ABGR8888,
    iBuf, aSurface^.w * 4
  ) then
  begin
    FreeMem(iBuf);
    raise TImageException.Create(SDL_GetError());
  end;

  Result := TImage.CreateAssign( iBuf, aSurface^.w, aSurface^.h);
  Result.RawX := aSurface^.w;
  Result.RawY := aSurface^.h;
end;

function LoadImage( const FileName : Ansistring ) : TImage;
var iSDLSurface : PSDL_Surface;
begin
  LoadSDL3Image;
  iSDLSurface := IMG_Load( PChar( FileName ) );
  if iSDLSurface = nil then
    raise TImageException.Create('LoadImage('+FileName+') : '+SDL_GetError()+' (png/jpg library or file missing?)' );
  Exit( LoadImage( iSDLSurface ) );
end;

function LoadImage( Stream : TStream; Size : DWord ): TImage;
var iSDLSurface : PSDL_Surface;
begin
  LoadSDL3Image;
  iSDLSurface := IMG_Load_IO( SDL_IOFromStream( Stream, Size, False, True ), True );
  if iSDLSurface = nil then
    raise TImageException.Create('LoadImage(RWOps) : '+SDL_GetError()+' (png/jpg library or file missing?)' );
  Exit( LoadImage( iSDLSurface ) );
end;

function UploadImage( Image: TImage; aBlend : Boolean ): DWord;
var GLTexture : DWord;
begin
  glGenTextures(1, @GLTexture);
  ReUploadImage( GLTexture, Image, aBlend );
  Exit( GLTexture );
end;

procedure ReUploadImage ( GLTexture : DWord; Image : TImage; aBlend : Boolean );
var GLBlend   : GLInt;
begin
  if aBlend
    then GLBlend := GL_LINEAR
    else GLBlend := GL_NEAREST;

  glBindTexture(GL_TEXTURE_2D, GLTexture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GLBlend );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GLBlend );
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
    Image.SizeX, Image.SizeY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Image.Data );
  glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure UnUploadImage( GLTexture : DWord );
begin
  if GLTexture <> 0 then
    glDeleteTextures(1, @GLTexture);
end;

end.

