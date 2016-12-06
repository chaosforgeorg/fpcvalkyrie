{$INCLUDE valkyrie.inc}
// @abstract(Message class unit for Valkyrie)
// @author(Kornel Kisielewicz <epyon@chaosforge.org>)
// @created(May 7, 2004)
// @lastmod(May 7, 2004)
//
// Implements the @link(TMessage) class.
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

unit vmsg;
interface
uses vutil;

// Basic message type. Other message types need to inherit from it.
type TMessage = class
       public
       // Message identification.
       ID : TMSGID;
       // Standard constructor.
       constructor Create(MSGID : TMSGID);
       // Standard destructor.
       destructor Destroy; override;
     end;

implementation

constructor TMessage.Create(MSGID : TMSGID);
begin
  ID := MSGID;
end;

destructor TMessage.Destroy;
begin
end;

end.

//* Created 22.08.2003
