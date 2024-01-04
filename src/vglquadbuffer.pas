{$INCLUDE valkyrie.inc}
unit vglquadbuffer;
interface

uses Classes, SysUtils, vgltypes, vgenerics, vvector, vglprogram, vgldrawarrays,
     vglquadarrays;

type TGLTexturedColoredQuadLayer = class( TGLTexturedArrays )
  function GetArray( aTextureID : Cardinal ) : TGLTexturedColoredQuads;
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



end.

