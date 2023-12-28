unit vgl3library;
{$include ../src/valkyrie.inc}
{$MACRO ON}
interface
uses Classes, SysUtils, vlibrary;

const
{$IFDEF WINDOWS}
  GL3DefaultPath = 'opengl32.dll';
{$ELSE}
  {$IFDEF DARWIN}
  GL3DefaultPath = '/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib';
  {$ELSE}
  GL3DefaultPath = 'libGL.so';
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE extdecl := cdecl}
{$ELSE}
  {$DEFINE extdecl := stdcall}
{$ENDIF}

{$include vgl15types.inc}
{$include vgl21types.inc}
{$include vgl33types.inc}
{$include vgl15vars.inc}
{$include vgl21vars.inc}
{$include vgl33vars.inc}

var
  glExtLoader : function( name : PGLchar ) : PGLvoid; extdecl;
  GL3         : TLibrary = nil;

function LoadGL3( const aPath : AnsiString = GL3DefaultPath ) : Boolean;
function LoadGL3Compat( const aPath : AnsiString = GL3DefaultPath ) : Boolean;

// deprecated Compatibility mode
const
  GL_PROJECTION                     = $1701;
  GL_MODELVIEW                      = $1700;
  GL_TEXTURE_COORD_ARRAY            = $8078;

var
  glMatrixMode  : procedure(mode: GLenum); extdecl;
  glMultMatrixf : procedure(const m: PGLfloat); extdecl;
  glLoadIdentity: procedure; extdecl;
  glPushMatrix  : procedure; extdecl;
  glOrtho       : procedure(left, right, bottom, top, zNear, zFar: GLdouble); extdecl;
  glColor4f     : procedure(red, green, blue, alpha: GLfloat); extdecl;
  glEnableClientState : procedure(aarray: GLenum); extdecl;
  glDisableClientState: procedure(aarray: GLenum); extdecl;
  glVertexPointer     : procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;
  glTexCoordPointer   : procedure(size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); extdecl;

implementation

uses math;

function LoadGL3 ( const aPath : AnsiString ) : Boolean;
  function GetSymbol( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbol := GL3.Get( PChar(aSymbol) );
    if GetSymbol = nil then
      raise ELibraryError.Create( 'GL3 : Symbol "'+aSymbol+'" not found!' );
  end;
  function GetSymbolExt( const aSymbol : AnsiString ) : Pointer;
  begin
    GetSymbolExt := glExtLoader( PChar(aSymbol) );
    if GetSymbolExt = nil then
      raise ELibraryError.Create( 'GL3 : SymbolExt "'+aSymbol+'" not found!' );
  end;
begin
  if GL3 <> nil then Exit( True );

  {$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
  {$endif}

  GL3 := TLibrary.Load( aPath );
  if GL3 = nil then Exit( False );

{$IFDEF UNIX}
  Pointer( glExtLoader ) := GetSymbol( 'glXGetProcAddress' );
{$ELSE}
  Pointer( glExtLoader ) := GetSymbol( 'wglGetProcAddress' );
{$ENDIF}

{$include vgl15calls.inc}
{$include vgl21calls.inc}
{$include vgl33calls.inc}

  Exit( True );
end;

function LoadGL3Compat( const aPath : AnsiString = GL3DefaultPath ) : Boolean;
function GetSymbol( const aSymbol : AnsiString ) : Pointer;
begin
  GetSymbol := GL3.Get( PChar(aSymbol) );
  if GetSymbol = nil then
    raise ELibraryError.Create( 'GL3Compat : Symbol "'+aSymbol+'" not found!' );
end;
begin
  if GL3 = nil then LoadGL3( aPath );
  if Assigned( glMatrixMode ) then Exit( True );

  Pointer( glMatrixMode )   := GetSymbol( 'glMatrixMode' );
  Pointer( glMultMatrixf )  := GetSymbol( 'glMultMatrixf' );
  Pointer( glLoadIdentity ) := GetSymbol( 'glLoadIdentity' );
  Pointer( glOrtho )        := GetSymbol( 'glOrtho' );
  Pointer( glPushMatrix )   := GetSymbol( 'glPushMatrix' );
  Pointer( glColor4f )      := GetSymbol( 'glColor4f' );

  Pointer( glEnableClientState )  := GetSymbol( 'glEnableClientState' );
  Pointer( glDisableClientState ) := GetSymbol( 'glDisableClientState' );
  Pointer( glVertexPointer )      := GetSymbol( 'glVertexPointer' );
  Pointer( glTexCoordPointer )    := GetSymbol( 'glTexCoordPointer' );

  Exit( True );
end;


finalization
  if GL3 <> nil then FreeAndNil( GL3 );

end.

