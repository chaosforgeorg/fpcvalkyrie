{$INCLUDE valkyrie.inc}
unit vglframebuffer;
interface
uses vgltypes, vgenerics;

type TGLFramebufferAttachment = class
  private
    FFormat     : TGLPixelFormat;
    FMultiplier : Single;
    FLinear     : Boolean;
    FTextureID  : Cardinal;
    FIndex      : Cardinal;
  public
    constructor Create( aIndex : Cardinal; aFormat : TGLPixelFormat; aLinear : Boolean; aMultiplier : Single );
    procedure Resize( aNewWidth, aNewHeight : Integer );
    destructor Destroy; override;
  public
    property TextureID : Cardinal read FTextureID;
end;

type TGLFramebufferAttachmentArray = specialize TGObjectArray< TGLFramebufferAttachment >;

type TGLFramebuffer = class
  private
    FWidth           : Integer;
    FHeight          : Integer;
    FFramebufferID   : Cardinal;
    FAttachments     : TGLFramebufferAttachmentArray;
    FDepthID         : Cardinal;
    FDepth           : Boolean;
  public
    constructor Create;
    procedure AddAttachment( aFormat : TGLPixelFormat = RGBA8; aLinear : Boolean = True; aMultiplier : Single = 1.0 );
    procedure AddDepthBuffer;
    destructor Destroy; override;
    procedure Resize( aNewWidth, aNewHeight: Integer );
    procedure BindAndClear;
    procedure Bind;
    procedure Unbind;
    function GetTextureID( aIndex: Integer = 0 ): Cardinal;
  end;

implementation

uses SysUtils, math, vgl3library;

constructor TGLFramebufferAttachment.Create( aIndex : Cardinal; aFormat : TGLPixelFormat; aLinear : Boolean; aMultiplier : Single );
begin
  FIndex      := aIndex;
  FFormat     := aFormat;
  FLinear     := aLinear;
  FMultiplier := aMultiplier;
  FTextureID  := 0;
end;

procedure TGLFramebufferAttachment.Resize( aNewWidth, aNewHeight : Integer );
begin
  if FMultiplier <> 1.0 then
  begin
    aNewWidth  := Ceil( aNewWidth * FMultiplier );
    aNewHeight := Ceil( aNewHeight * FMultiplier );
  end;
  if FTextureID <> 0 then glDeleteTextures( 1, @FTextureID );
  glGenTextures( 1, @FTextureID );
  glBindTexture( GL_TEXTURE_2D, FTextureID );
  glTexImage2D( GL_TEXTURE_2D, 0, GLPixelFormatToInternalEnum( FFormat ),
    aNewWidth, aNewHeight, 0, GLPixelFormatToEnum( FFormat ),
    GLPixelFormatToType( FFormat ), nil);
  if FLinear then
  begin
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
  end
  else
  begin
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
  end;

  glFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + FIndex, GL_TEXTURE_2D, FTextureID, 0 );
  glBindTexture( GL_TEXTURE_2D, 0 );
end;

destructor TGLFramebufferAttachment.Destroy;
begin
  if FTextureID <> 0 then glDeleteTextures( 1, @FTextureID );
  inherited Destroy;
end;

constructor TGLFramebuffer.Create;
begin
  FAttachments := TGLFramebufferAttachmentArray.Create;
  FFramebufferID := 0;
  FDepthID       := 0;
  FWidth         := 0;
  FHeight        := 0;
  FDepth         := False;
end;

procedure TGLFramebuffer.AddAttachment( aFormat : TGLPixelFormat = RGBA8; aLinear : Boolean = True; aMultiplier : Single = 1.0 );
begin
  FAttachments.Push( TGLFramebufferAttachment.Create( FAttachments.Size, aFormat, aLinear, aMultiplier ) );
end;

procedure TGLFramebuffer.AddDepthBuffer;
begin
  if FDepth then raise Exception.Create('Framebuffer can''t have more than one depth attachment!');
  FDepth         := True;
end;

procedure TGLFramebuffer.Resize( aNewWidth, aNewHeight: Integer );
var i        : Integer;
    iBuffers : array of Cardinal;
begin
  if ( aNewWidth = FWidth ) and ( aNewHeight = FHeight ) then Exit;
  if FAttachments.Size < 1 then
    raise Exception.Create('Framebuffer must have at least one color attachment.');

  if ( FDepthID <> 0 ) then
    glDeleteRenderbuffers( 1, @FDepthID );

  if FFramebufferID = 0 then
    glGenFramebuffers(1, @FFramebufferID);
  glBindFramebuffer( GL_FRAMEBUFFER, FFramebufferID );

  SetLength( iBuffers, FAttachments.Size );
  for i := 0 to FAttachments.Size - 1 do
  begin
    FAttachments[i].Resize( aNewWidth, aNewHeight );
    iBuffers[i] := GL_COLOR_ATTACHMENT0 + i;
  end;

  if FDepth then
  begin
    glGenRenderbuffers( 1, @FDepthID );
    glBindRenderbuffer( GL_RENDERBUFFER, FDepthID );
    glRenderbufferStorage( GL_RENDERBUFFER, GL_DEPTH_COMPONENT, aNewWidth, aNewHeight );
    glFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthID );
  end;

  glDrawBuffers( FAttachments.Size, @iBuffers[0] );

  if ( glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE ) then
    raise Exception.Create('Failed to resize framebuffer to complete state');

  FWidth := aNewWidth;
  FHeight := aNewHeight;

  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

destructor TGLFramebuffer.Destroy;
begin
  if FFramebufferID <> 0 then
    glDeleteFramebuffers( 1, @FFramebufferID );
  FreeAndNil( FAttachments );
  if ( FDepthID <> 0 ) then
    glDeleteRenderbuffers( 1, @FDepthID );
  inherited Destroy;
end;

procedure TGLFramebuffer.Bind;
begin
  if FFramebufferID = 0 then raise Exception.Create('Framebuffer not created!');
  glBindFramebuffer( GL_FRAMEBUFFER, FFramebufferID );
  glViewport( 0, 0, FWidth, FHeight );
end;

procedure TGLFramebuffer.BindAndClear;
begin
  if FFramebufferID = 0 then raise Exception.Create('Framebuffer not created!');
  glBindFramebuffer( GL_FRAMEBUFFER, FFramebufferID );
  glViewport( 0, 0, FWidth, FHeight );
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
end;

procedure TGLFramebuffer.Unbind;
begin
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

function TGLFramebuffer.GetTextureID( aIndex : Integer = 0 ): Cardinal;
begin
  if FFramebufferID = 0 then raise Exception.Create('Framebuffer not created!');
  if ( aIndex < 0 ) or ( aIndex >= FAttachments.Size ) then
    raise Exception.Create('Invalid attachment index.');
  Result := FAttachments[aIndex].TextureID;
end;

end.

