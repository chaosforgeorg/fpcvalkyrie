{$INCLUDE valkyrie.inc}
unit vtigstyle;
interface

uses viotypes;

type TTIGStyleColorEntry = (
  VTIG_TEXT_COLOR,
  VTIG_BOLD_COLOR,
  VTIG_TITLE_COLOR,
  VTIG_FOOTER_COLOR,
  VTIG_FRAME_COLOR,
  VTIG_SCROLL_COLOR,
  VTIG_DISABLED_COLOR,
  VTIG_BACKGROUND_COLOR,
  VTIG_WINDOW_BACKGROUND_COLOR,
  VTIG_INPUT_TEXT_COLOR,
  VTIG_INPUT_BACKGROUND_COLOR,
  VTIG_SELECTED_TEXT_COLOR,
  VTIG_SELECTED_UNSELECTED_COLOR,
  VTIG_SELECTED_DISABLED_COLOR,
  VTIG_SELECTED_BACKGROUND_COLOR
);

type TTIGStyleFrameEntry = (
  VTIG_BORDER_FRAME,
  VTIG_RULER_FRAME,
  VTIG_GROUP_FRAME,
  VTIG_SCROLL_FRAME
);

type TTIGStylePaddingEntry = (
  VTIG_WINDOW_PADDING,
  VTIG_SELECTABLE_PADDING,
  VTIG_GROUP_PADDING,
  VTIG_GROUP_FRAME_PADDING
);

type TTIGStyle = record
  Color   : array[ TTIGStyleColorEntry ]   of TIOColor;
  Frame   : array[ TTIGStyleFrameEntry ]   of Ansistring;
  Padding : array[ TTIGStylePaddingEntry ] of TIOPoint;
end;

type PTIGStyle = ^TTIGStyle;

var VTIGDefaultStyle : TTIGStyle;

implementation

uses vutil;

initialization

FillChar( VTIGDefaultStyle, SizeOf( VTIGDefaultStyle ), 0 );
VTIGDefaultStyle.Color[ VTIG_TEXT_COLOR ]                := LightGray;
VTIGDefaultStyle.Color[ VTIG_BOLD_COLOR ]                := White;
VTIGDefaultStyle.Color[ VTIG_TITLE_COLOR ]               := White;
VTIGDefaultStyle.Color[ VTIG_FOOTER_COLOR ]              := White;
VTIGDefaultStyle.Color[ VTIG_FRAME_COLOR ]               := DarkGray;
VTIGDefaultStyle.Color[ VTIG_SCROLL_COLOR ]              := DarkGray;
VTIGDefaultStyle.Color[ VTIG_DISABLED_COLOR ]            := DarkGray;
VTIGDefaultStyle.Color[ VTIG_BACKGROUND_COLOR ]          := Black;
VTIGDefaultStyle.Color[ VTIG_WINDOW_BACKGROUND_COLOR ]   := Black;
VTIGDefaultStyle.Color[ VTIG_INPUT_TEXT_COLOR ]          := Yellow;
VTIGDefaultStyle.Color[ VTIG_INPUT_BACKGROUND_COLOR ]    := DarkGray;
VTIGDefaultStyle.Color[ VTIG_SELECTED_TEXT_COLOR ]       := White;
VTIGDefaultStyle.Color[ VTIG_SELECTED_UNSELECTED_COLOR ] := DarkGray;
VTIGDefaultStyle.Color[ VTIG_SELECTED_DISABLED_COLOR ]   := DarkGray;
VTIGDefaultStyle.Color[ VTIG_SELECTED_BACKGROUND_COLOR ] := LightGray;

VTIGDefaultStyle.Frame[ VTIG_BORDER_FRAME ] := #196+#196+#179+#179+#218+#191+#192+#217;
VTIGDefaultStyle.Frame[ VTIG_RULER_FRAME ]  := #196+#179;
VTIGDefaultStyle.Frame[ VTIG_GROUP_FRAME ]  := #196+#179;
VTIGDefaultStyle.Frame[ VTIG_SCROLL_FRAME ] := #193+#194+#177+#178;

VTIGDefaultStyle.Padding[ VTIG_WINDOW_PADDING ]     := Point( 1,1 );
VTIGDefaultStyle.Padding[ VTIG_SELECTABLE_PADDING ] := Point( 1,0 );
VTIGDefaultStyle.Padding[ VTIG_GROUP_PADDING ]      := Point( 2,0 );
VTIGDefaultStyle.Padding[ VTIG_GROUP_FRAME_PADDING ]:= Point( 0,0 );
end.

