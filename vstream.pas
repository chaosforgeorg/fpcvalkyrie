{$INCLUDE valkyrie.inc}
// @abstract(Generic data structures for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @cvs($Author: chaos-dev $)
//
// TODO: implement support for streaming Dynamic Arrays
// TODO: implement support for streaming Variants
// TODO: implement support for streaming Records
// TODO: implement support for streaming other string types
unit vstream;
interface

uses Classes, SysUtils, TypInfo;

type TStreamHelper = class helper for TStream
  procedure WriteType( aPointer : Pointer; aSize : DWord; aTypeInfo : PTypeInfo );
  procedure ReadType( aPointer : Pointer; aSize : DWord; aTypeInfo : PTypeInfo );
  function ReadObject( aClassType : TClass ) : TObject;
  procedure WriteObject( aObject : TObject );
  procedure WriteVariant( aValue : Variant );
  function ReadVariant : Variant;
  procedure WriteBool( aBool : Boolean );
  function ReadBool : Boolean;
end;

implementation

uses vnode;

procedure TStreamHelper.WriteType( aPointer : Pointer; aSize : DWord; aTypeInfo : PTypeInfo );
begin
  case aTypeInfo^.Kind of
    tkInteger,tkChar,tkEnumeration, tkObject,
    tkFloat,tkSet,tkSString,tkWChar,
    tkBool,tkInt64,tkQWord : Write( aPointer^, aSize );
    tkAString : WriteAnsiString( AnsiString(aPointer^) );
    tkClass : WriteObject( TObject(aPointer^) );
    tkVariant : WriteVariant( Variant(aPointer^) );
    tkMethod, tkInterface, tkArray, tkRecord,tkLString,tkWString,
    tkDynArray, tkInterfaceRaw,tkProcVar,tkUString,tkUChar,
    tkHelper,tkUnknown : raise Exception.Create('TStreamHelper.WriteType - don''t know how to stream type "'+aTypeInfo^.Name+'/'+IntToStr( QWord(aTypeInfo^.Kind) )+'"!');
  end;
end;

procedure TStreamHelper.ReadType( aPointer : Pointer; aSize : DWord; aTypeInfo : PTypeInfo );
begin
  case aTypeInfo^.Kind of
    tkInteger,tkChar,tkEnumeration, tkObject,
    tkFloat,tkSet,tkSString,tkWChar,
    tkBool,tkInt64,tkQWord : Read( aPointer^, aSize );
    tkAString : AnsiString(aPointer^) := ReadAnsiString();
    tkClass : TObject(aPointer^) := ReadObject( GetTypeData( aTypeInfo )^.ClassType );
    tkVariant : Variant(aPointer^) := ReadVariant;
    tkMethod, tkInterface, tkArray, tkRecord,tkLString,tkWString,
    tkDynArray, tkInterfaceRaw,tkProcVar,tkUString,tkUChar,
    tkHelper,tkUnknown : raise Exception.Create('TStreamHelper.ReadType - don''t know how to stream type "'+aTypeInfo^.Name+'"!');
  end;
end;

function TStreamHelper.ReadObject( aClassType : TClass ) : TObject;
begin
  if aClassType.InheritsFrom( TVObject ) then
  begin
    Exit( TVObjectClass(aClassType).CreateFromStream( Self ) );
  end;
  raise Exception.Create('TStreamHelper.ReadObject - unsupported object type "'+aClassType.ClassName+'"!');
end;

procedure TStreamHelper.WriteObject( aObject : TObject );
begin
  if aObject.InheritsFrom( TVObject ) then
  begin
    TVObject(aObject).WriteToStream( Self );
  end
  else
  raise Exception.Create('TStreamHelper.WriteObject - unsupported object type "'+aObject.ClassName+'"!');
end;

procedure TStreamHelper.WriteVariant( aValue : Variant );
begin
  with TVarData(aValue) do
  begin
    WriteWord( vType );
    case vType of
      varnull     : ;
      varsmallint : Write(vSmallInt,SizeOf(vSmallInt));
      varshortint : Write(vShortInt,SizeOf(vShortInt));
      varinteger  : Write(vInteger,SizeOf(vInteger));
      varint64    : Write(vInt64,SizeOf(vInt64));
      varsingle   : Write(vSingle,SizeOf(vSingle));
      vardouble   : Write(vDouble,SizeOf(vDouble));
      varboolean  : Write(vBoolean,SizeOf(vBoolean));
      varbyte     : Write(vByte,SizeOf(vByte));
      varword     : Write(vWord,SizeOf(vWord));
      varlongword : Write(vLongWord,SizeOf(vLongWord));
      varqword    : Write(vQWord,SizeOf(vQWord));
      varstring   : WriteAnsiString(aValue);
    else
      raise Exception.Create('WriteVariant : unsupported variant type ('+IntToStr(vType)+')!');
    end;
  end;
end;

{$HINTS OFF}
function TStreamHelper.ReadVariant : Variant;
var VType : Word;
    S     : Single;
    D     : Double;
    B     : Boolean;
begin
  VType := ReadWord;
  case vType of
    varnull     : Exit( varNull );
    varshortint : Exit( ShortInt(ReadByte) );
    varsmallint : Exit( SmallInt(ReadWord) );
    varinteger  : Exit( Integer(ReadDWord) );
    varint64    : Exit( Int64(ReadQWord) );
    varsingle   : begin Read(S,SizeOf(Single)); Exit(S); end;
    vardouble   : begin Read(D,SizeOf(Double)); Exit(D); end;
    varboolean  : begin Read(B,SizeOf(Boolean)); Exit(B); end;
    varbyte     : Exit( ReadByte );
    varword     : Exit( ReadWord );
    varlongword : Exit( ReadDWord );
    varqword    : Exit( ReadQWord );
    varstring   : Exit( ReadAnsiString );
  else
    raise Exception.Create('ReadVariant : unsupported variant type ('+IntToStr(vType)+')!');
  end;

end;
{$HINTS ON}

procedure TStreamHelper.WriteBool( aBool : Boolean );
begin
  if aBool then WriteByte( 1 ) else WriteByte( 0 );
end;

function TStreamHelper.ReadBool : Boolean;
begin
  Exit( ReadByte > 0 );
end;

end.

