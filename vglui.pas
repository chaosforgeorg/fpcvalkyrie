{$INCLUDE valkyrie.inc}
unit vglui;
interface
uses Classes, SysUtils, vutil, viotypes, vioevent, vioconsole, vuitypes,
     vuielement, vuielements, vgltypes, vglquadsheet, vbitmapfont;

type TGLUILabel      = class( TUICustomLabel )
  constructor Create( aParent : TUIElement; aTarget : TGLQuadSheet; aFont : TBitmapFont; const aPos : TUIPoint; const aText : TUIString );
  procedure OnRedraw; override;
  procedure SetText( const aText : TUIString ); override;
protected
  FQuadList : TGLTexturedQuadList;
  FTarget   : TGLQuadSheet;
  FBFont    : TBitmapFont;
  FGLPos    : TGLVec2i;
end;

type TGLUIMessages      = class( TUICustomMessages )
  constructor Create( aParent : TUIElement; aTarget : TGLQuadSheet; aFont : TBitmapFont; const aPos : TUIPoint ); reintroduce;
  procedure Add( const aMessage : Ansistring ); override;
  procedure Update; override;
  procedure Clear; override;
  procedure OnRedraw; override;
protected
  function Chunkify( const aString : AnsiString; aStart : Integer; aColor : TUIColor = ColorNone ) : TUIChunkBuffer; override;
protected
  FQuadList  : TGLTexturedQuadList;
  FLines     : DWord;
  FTarget    : TGLQuadSheet;
  FBFont     : TBitmapFont;
  FGLPos     : TGLVec2i;
end;

procedure TextToQuadList ( aList : TGLTexturedQuadList; const aText : AnsiString; aColor : TGLVec4f; aPosition : TGLVec2i; aFont : TBitmapFont; aSize : Byte = 1 );

implementation

uses vlog;

function UIColorToGLColor( aColor : TUIColor ) : TGLVec4f;
begin
  if aColor < 16 then Exit( GLFloatColors4[ aColor ] );
end;

procedure TextToQuadList ( aList : TGLTexturedQuadList; const aText : AnsiString; aColor : TGLVec4f;
  aPosition : TGLVec2i; aFont : TBitmapFont; aSize : Byte = 1 );
var iPosition : TGLVec2i;
    iGPos     : TGLVec2i;
    iCount    : DWord;
    iTexQuad  : TGLRawQTexCoord;
    iSize     : TGLVec2i;
    iLength   : DWord;
begin
  iPosition := aPosition;
  iSize.Init( aSize, aSize );
  iLength := Length( aText );
  if iLength <> 0 then
  for iCount := 1 to iLength do
  begin
    aFont.SetTexCoord( iTexQuad, aText[iCount] );
    iGPos := aFont.GetPosition( aText[iCount] );
    aList.PostQuad( iPosition + iGPos, iPosition + iGPos + aFont.GetSize( aText[iCount] ) * iSize, TGLVec4f.Create(1,1,0,1){aColor}, iTexQuad );
    if iCount < iLength then
      iPosition.X := iPosition.X + aFont.GetKerning( aText[iCount], aText[iCount+1] );
  end;
end;

{ TGLUIMessages }

constructor TGLUIMessages.Create( aParent : TUIElement; aTarget : TGLQuadSheet; aFont : TBitmapFont; const aPos : TUIPoint );
begin
  inherited Create( aParent, aParent.GetAvailableDim, 2, nil );
  FBFont := aFont;
  FTarget := aTarget;
  FQuadList := TGLTexturedQuadList.Create;
  FGLPos.Init( 10, 10 );
  FLines := 0;
end;

procedure TGLUIMessages.Add ( const aMessage : Ansistring ) ;
begin
  TextToQuadList( FQuadList, aMessage, UIColorToGLColor( FForeColor ), FGLPos + TGLVec2i.Create( 0, FLines * ( 2 + FBFont.GylphSize.Y ) ), FBFont, 1 );
  Inc( FLines );
end;

procedure TGLUIMessages.Update;
begin
  FQuadList.Clear;
  FLines := 0;
end;

procedure TGLUIMessages.Clear;
begin
  FQuadList.Clear;
  FLines := 0;
end;

procedure TGLUIMessages.OnRedraw;
begin
  inherited OnRedraw;
  if FTarget = nil then Exit;
  FTarget.Append( FQuadList, FBFont.GLTexture );
end;

function TGLUIMessages.Chunkify ( const aString : AnsiString; aStart : Integer;
  aColor : TUIColor ) : TUIChunkBuffer;
begin
  Exit( nil );
end;

{ TGLUILabel }

constructor TGLUILabel.Create ( aParent : TUIElement; aTarget : TGLQuadSheet; aFont : TBitmapFont; const aPos : TUIPoint; const aText : TUIString );
begin
  FQuadList := nil;
  FTarget   := aTarget;
  FBFont     := aFont;
  FGLPos.Init( aPos.X, aPos.Y );
  inherited Create( aParent, Rectangle( aPos, Length(aText), 1 ), aText );
  FDirty := True;
end;

procedure TGLUILabel.OnRedraw;
begin
  if FQuadList = nil then
  begin
    FQuadList := TGLTexturedQuadList.Create;
    SetText( FText );
  end;
  inherited OnRedraw;
  if FTarget = nil then Exit;
  FTarget.Append( FQuadList, FBFont.GLTexture );
end;

procedure TGLUILabel.SetText ( const aText : TUIString ) ;
begin
  inherited SetText ( aText );
  if (FQuadList = nil) or (FBFont = nil) then Exit;
  FQuadList.Clear;
  TextToQuadList( FQuadList, aText, UIColorToGLColor( FForeColor ), FGLPos, FBFont, 1 );
end;

end.

