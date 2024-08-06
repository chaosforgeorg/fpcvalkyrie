{$INCLUDE valkyrie.inc}
unit vglframebuffer;
interface

type TGLFramebuffer = class
  private
    FWidth         : Integer;
    FHeight        : Integer;
    FFramebufferID : Cardinal;
    FTextureID     : Cardinal;
  public
    constructor Create( Width, Height: Integer );
    destructor Destroy; override;
    procedure Resize( NewWidth, NewHeight: Integer );
    procedure BindAndClear;
    procedure Bind;
    procedure Unbind;
    function GetTextureID: Cardinal;
  end;

implementation

uses SysUtils, vgl3library;

{ TFramebuffer }

constructor TGLFramebuffer.Create( Width, Height: Integer );
begin
  glGenFramebuffers(1, @FFramebufferID);
  glGenTextures(1, @FTextureID); // Ensure a texture ID is generated before first resize
  FWidth := 0; // Initialize to ensure the first resize operation proceeds
  FHeight := 0;
  Resize( Width, Height );
end;

procedure TGLFramebuffer.Resize( NewWidth, NewHeight: Integer );
begin
  if ( NewWidth = FWidth ) and ( NewHeight = FHeight ) then Exit; // No resize needed
  if ( FWidth <> 0 ) then
  begin
    glDeleteTextures( 1, @FTextureID );
    glGenTextures(1, @FTextureID);
  end;

  // Bind the framebuffer to update its attachments
  glBindFramebuffer(GL_FRAMEBUFFER, FFramebufferID);

  // Re-create texture with new dimensions
  glBindTexture(GL_TEXTURE_2D, FTextureID);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, NewWidth, NewHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // Attach the new texture to the framebuffer
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FTextureID, 0);
  glBindTexture(GL_TEXTURE_2D, 0);

  // Check if the framebuffer is still complete
  if (glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE) then
    raise Exception.Create('Failed to resize framebuffer to complete state');

  FWidth := NewWidth;
  FHeight := NewHeight;

  // Unbind the framebuffer
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;


destructor TGLFramebuffer.Destroy;
begin
  if FFramebufferID <> 0 then
    glDeleteFramebuffers(1, @FFramebufferID);
  if FTextureID <> 0 then
    glDeleteTextures(1, @FTextureID);
  inherited Destroy;
end;

procedure TGLFramebuffer.Bind;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, FFramebufferID);
  glViewport(0, 0, FWidth, FHeight);
end;

procedure TGLFramebuffer.BindAndClear;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, FFramebufferID);
  glViewport(0, 0, FWidth, FHeight);
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
end;

procedure TGLFramebuffer.Unbind;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

function TGLFramebuffer.GetTextureID : Cardinal;
begin
  Result := FTextureID;
end;

end.

