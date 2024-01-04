{$INCLUDE valkyrie.inc}
unit vglquadbuffer;
interface

uses Classes, SysUtils, vgltypes, vgenerics, vvector, vglprogram, vgldrawarrays,
     vglquadarrays;

type TGLTexturedColoredQuadLayer = class( TGLTexturedArrays )
  function GetArray( aTextureID : Cardinal ) : TGLTexturedColoredQuads;
  procedure Draw;
public
  property Quads[ aTextureID : Cardinal ]  : TGLTexturedColoredQuads read GetArray; default;
end;

implementation

uses vgl3library;

{ TGLTexturedColoredQuadLayer }

function TGLTexturedColoredQuadLayer.GetArray(aTextureID: Cardinal ): TGLTexturedColoredQuads;
begin
  Result := TGLTexturedColoredQuads( GetDrawArray( aTextureID ) );
  if Result = nil then
  begin
    Result := TGLTexturedColoredQuads.Create;
    FDrawArrays.Push( Result );
    FTextureIDs.Push( aTextureID );
  end;
end;

procedure TGLTexturedColoredQuadLayer.Draw;
var i : Integer;
begin
  if FDrawArrays.Size = 0 then Exit;

  for i := 0 to FDrawArrays.Size - 1 do
    FDrawArrays[i].Update;

  for i := 0 to FDrawArrays.Size - 1 do
  begin
    glActiveTexture( GL_TEXTURE0 );
    glBindTexture( GL_TEXTURE_2D, FTextureIDs[i]  );
    FDrawArrays[i].Draw;
  end;

  for i := 0 to FDrawArrays.Size - 1 do
    FDrawArrays[i].Clear;
end;


end.

