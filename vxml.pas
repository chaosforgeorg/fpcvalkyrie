{$INCLUDE valkyrie.inc}
// @abstract(XML utilities wrapper class for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
// @cvs($Author: chaos-dev $)
// @cvs($Date: 2008-01-14 22:16:41 +0100 (Mon, 14 Jan 2008) $)
//
//  @html <div class="license">
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or (at your
//  option) any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
//  for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this library; if not, write to the Free Software Foundation,
//  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//  @html </div>

unit vxml;
interface
uses DOM, XPath;

type

{ TVXMLDocument }

TVXMLDocument = class(TXMLDocument)
  function GetElement( aXPathQuery: string; aContext: TDOMNode = nil ): TDOMElement;
  function GetAttribute( aXPathQuery, aAttribute: string; aContext: TDOMNode = nil ): string;
  function CRC( const aKey1, aKey2 : AnsiString ) : AnsiString;
end;

implementation

uses MD5, sysutils;

function TVXMLDocument.GetElement( aXPathQuery: string; aContext: TDOMNode ): TDOMElement;
var iXPathResult : TXPathVariable;
begin
try
  if aContext = nil then aContext := Self;
  iXPathResult := EvaluateXPathExpression(aXPathQuery, aContext);
  if (iXPathResult = nil) or (iXPathResult.AsNodeSet().First = nil) then
  begin
    FreeAndNil(iXPathResult);
    Exit(nil);
  end;
  GetElement := TDOMElement(iXPathResult.AsNodeSet().First);
  FreeAndNil( iXPathResult );
except
  on e : Exception do
  begin
    e.Message := e.Message + ' ("'+aXPathQuery+'")';
    raise;
  end;
end;
end;

function TVXMLDocument.GetAttribute(aXPathQuery, aAttribute: string; aContext: TDOMNode) : string;
var iXMLElement  : TDOMElement;
begin
  if aContext = nil then aContext := Self;
  iXMLElement := GetElement( aXPathQuery, aContext );
  if (iXMLElement = nil) then Exit('') else Exit( iXMLElement.GetAttribute(aAttribute) );
end;

function TVXMLDocument.CRC(const aKey1, aKey2: AnsiString): AnsiString;
var iContext : TMDContext;
    iNode    : TDOMNode;
    iDigest  : TMDDigest;
    iMark    : AnsiString;
  procedure ProcessNode( aNode : TDOMNode );
  var
    iRecNode  : TDOMNode;
    iCount    : DWord;
    iString   : AnsiString;
  begin
    if aNode = nil then Exit;

    if aNode.Attributes.Length <> 0 then
    for iCount := 0 to aNode.Attributes.Length - 1 do
    begin
      iString := aNode.Attributes[iCount].NodeValue;
      MDUpdate(iContext, PChar(iString)^, length(iString));
    end;

    iRecNode := aNode.FirstChild;
    while iRecNode <> nil do
    begin
      ProcessNode( iRecNode );
      iRecNode := iRecNode.NextSibling;
    end;
  end;
begin
  iMark := aKey1;
  MDInit( iContext, MD_VERSION_5 );
  iNode := DocumentElement.FirstChild;
  while iNode <> nil do
  begin
    ProcessNode(iNode); // Recursive
    iNode := iNode.NextSibling;
  end;
  MDUpdate( iContext, PChar(iMark)^, length(iMark));
  iMark := aKey2;
  MDUpdate( iContext, PChar(iMark)^, length(iMark));
  {$HINT OFF}
  MDFinal( iContext, iDigest );
  {$HINT ON}
  CRC := MD5Print( iDigest );
end;



end.

