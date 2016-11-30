{$INCLUDE valkyrie.inc}
unit vglprogram;
interface

uses Classes, SysUtils, vgltypes;

type

{ TGLProgram }

 TGLProgram = class
  constructor Create( const aVSSource, aFSSource : Ansistring );
  procedure Bind;
  function GetAttribLocation( const aName : AnsiString ) : Integer;
  function GetUniformLocation( const aName : AnsiString ) : Integer;
  procedure UnBind;
  destructor Destroy; override;
private
  procedure Compile( const aVSSource, aFSSource : Ansistring );
  function CompileShader( aType : Cardinal; const aSource : Ansistring ) : Cardinal;
private
  FProgramID : Cardinal;
  FVShaderID : Cardinal;
  FFShaderID : Cardinal;
end;

function SlurpFile( const aFileName : AnsiString ) : AnsiString;

implementation

uses vdebug, vgl2library;

function SlurpFile(const aFileName: AnsiString): AnsiString;
var
  iFileStream : TFileStream;
begin
  Result := '';
  with TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite) do
  try
    if Size > 0 then
    begin
      SetLength(Result, Size);
      Read(Pointer(Result)^, Size);
    end;
  finally
    Free;
  end;
end;

{ TGLProgram }

constructor TGLProgram.Create(const aVSSource, aFSSource: Ansistring);
begin
  FProgramID := 0;
  FVShaderID := 0;
  FFShaderID := 0;
  Compile( aVSSource, aFSSource );
end;

procedure TGLProgram.Bind;
begin
  glUseProgram( FProgramID );
end;

procedure TGLProgram.UnBind;
begin
  glUseProgram( 0 );
end;

function TGLProgram.GetAttribLocation(const aName: AnsiString): Integer;
begin
  Result := glGetAttribLocation( FProgramID, PChar(aName) );
  if Result = -1 then
    Log( 'Could not bind attribute '+aName+'!');
end;

function TGLProgram.GetUniformLocation(const aName: AnsiString): Integer;
begin
  Result := glGetUniformLocation( FProgramID, PChar(aName) );
  if Result = -1 then
    Log( 'Could not bind attribute '+aName+'!');
end;

destructor TGLProgram.Destroy;
begin
  inherited Destroy;
end;

procedure TGLProgram.Compile(const aVSSource, aFSSource: Ansistring);
var iCompileOK : LongInt;
    iLength    : LongInt;
    iBuffer    : AnsiString;
begin
  Log( 'Compiling program...' );
  iCompileOK := GL_FALSE;
  FVShaderID := CompileShader( GL_VERTEX_SHADER, aVSSource );
  FFShaderID := CompileShader( GL_FRAGMENT_SHADER, aFSSource );
  if (FVShaderID = 0) or (FFShaderID = 0) then Exit;

  FProgramID := glCreateProgram();
  glAttachShader( FProgramID, FVShaderID );
  glAttachShader( FProgramID, FFShaderID );
  glLinkProgram( FProgramID );
  glGetProgramiv( FProgramID, GL_LINK_STATUS, @iCompileOK);
  if iCompileOK = 0 then
  begin
    SetLength( iBuffer, 1024 );
    glGetProgramInfoLog( FProgramID, 1024, @iLength, PChar(iBuffer) );
    SetLength( iBuffer, iLength );
    Log( 'glLinkProgram failure : '+ iBuffer );
  end;
end;

function TGLProgram.CompileShader( aType: Cardinal; const aSource: Ansistring ): Cardinal;
var iCompileOK : LongInt;
    iLength    : LongInt;
    iShaderID  : Cardinal;
    iPSource   : PChar;
    iPPSource  : PPChar;
    iBuffer    : AnsiString;
begin
  Log( 'Compiling shader...' );
  iCompileOK := GL_FALSE;
  iShaderID  := glCreateShader( aType );
  iPSource   := PChar(aSource);
  iPPSource  := @iPSource;
  glShaderSource( iShaderID, 1, iPPSource, nil );
  glCompileShader( iShaderID );
  glGetShaderiv( iShaderID, GL_COMPILE_STATUS, @iCompileOK );
  if iCompileOK = 0 then
  begin
    SetLength( iBuffer, 1024 );
    glGetShaderInfoLog( iShaderID, 1024, @iLength, PChar(iBuffer) );
    SetLength( iBuffer, iLength );
    Log( 'Shader compile failure : '+ iBuffer );
    Exit(0);
  end;
  Log( 'Shader compiled successfuly.' );
  Exit( iShaderID );
end;

end.

