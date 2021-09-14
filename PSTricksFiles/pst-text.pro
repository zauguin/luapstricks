%!
% $Id: pst-text.pro 891 2018-12-29 19:42:20Z herbert $
%
% PostScript header file pst-text.pro
% Version 1.02, 2018-12-28 (hv)
% For distribution, see pstricks.tex.
%
/tx@TextPathDict 45 dict def
tx@TextPathDict begin
%
% Syntax:  <dist> PathPosition -
% Function: Searches for position of currentpath distance <dist> from
%           beginning. Sets (X,Y)=position, and Angle=tangent.
/PathPosition
{ /targetdist exch def
  /pathdist 0 def
  /continue true def
  /X { newx } def /Y { newy } def /Angle 0 def
  gsave
    flattenpath
    { movetoproc }  { linetoproc } { } { firstx firsty linetoproc }
    /pathforall load stopped { pop pop pop pop /X 0 def /Y 0 def } if
  grestore
} def
%
/movetoproc { continue { @movetoproc } { pop pop } ifelse } def
%
/@movetoproc
{ /newy exch def /newx exch def
  /firstx newx def /firsty newy def
} def
%
/linetoproc { continue { @linetoproc } { pop pop } ifelse } def
%
/@linetoproc {
  /oldx newx def /oldy newy def
  /newy exch def /newx exch def
  /dx newx oldx sub def
  /dy newy oldy sub def
  /dist dx dup mul dy dup mul add sqrt def
  /pathdist pathdist dist add def
  pathdist targetdist ge
  { pathdist targetdist sub dist div dup
    dy mul neg newy add /Y exch def
    dx mul neg newx add /X exch def
    /Angle dy dx atan def
    /continue false def
  } if
} bind def
%
/TextPathShow { 
  /String exch def
  /CharCount 0 def
% hv begin 2005-11-29   1.00
%   String length
%   { String CharCount 1 getinterval ShowChar
%     /CharCount CharCount 1 add def
   /CharSize 1 def
   currentfont /FontType get 0 eq
   { currentfont /FMapType get dup 2 eq exch dup 5 eq exch 9 eq or or
     { /CharSize 2 def} if
   } if
   String length CharSize idiv
   { String CharCount CharSize getinterval ShowChar
     /CharCount CharCount CharSize add def
% hv end 2005-11-29   1.00
  } repeat
} def
%
% Syntax: <TeX box resource> <vertical offset> <pathlength> <position> LuaTextPath -
/LuaTextPath {
  gsave
    currentpoint /Y exch def /X exch def
    exch X Hoffset sub sub mul
    Voffset Hoffset sub add
    neg /Hoffset exch def
    /Voffset exch def
  grestore
  gsave
  { % The stack contains: mark [...] op x y dx dy id, where id is the type of the node, dx the advance width, dy is 0, (x, y) the offset and op an operator drawing the node to coordinates (0, 0)
    gsave
    pop % Ignore the type of the Node
    2 div /Sy exch def
    2 div /Sx exch def
    Voffset sub Sy add exch
    Hoffset sub Sx add
    Transform
    Sx neg Sy neg translate
    currentdict exch end exec begin
    cleartomark % Should always come at the end because later versions might push additional entries.
    grestore
  }
  .texboxforall
  grestore
} def
%
% Syntax: <pathlength> <position> InitTextPath -
/InitTextPath
{ gsave
    currentpoint /Y exch def /X exch def
    exch X Hoffset sub sub mul
    Voffset Hoffset sub add
    neg X add /Hoffset exch def
    /Voffset Y def
  grestore
} def
%
/Transform
{ PathPosition
  dup
  Angle cos mul Y add exch
  Angle sin mul neg X add exch
  translate
  Angle rotate
} def
%
/ShowChar { 
  /Char exch def
  gsave
    Char end stringwidth
    tx@TextPathDict begin
    2 div /Sy exch def 2 div /Sx exch def
%
%%%  MV 10-09-99 00:36
    /sc?currentpoint where {pop sc?currentpoint} {currentpoint} ifelse
%   currentpoint
    Voffset sub Sy add exch
    Hoffset sub Sx add
    Transform
    Sx neg Sy neg moveto
    Char end tx@TextPathSavedShow
    tx@TextPathDict begin
  grestore
  Sx 2 mul Sy 2 mul rmoveto
} def
%
/warp {
    1 index
    1.4 mul
    cos 2 add
    mul
} def
/warpmove{
    2 index {
        newpath
    } if
    warp moveto
    pop false
} def
/warpline { warp lineto } def
/warpcurve {
    6 2 roll warp
    6 2 roll warp
    6 2 roll warp
    curveto
} def
/warpit {
    true
    { warpmove } { warpline } { warpcurve } { closepath } pathforall
    pop
} def
%
/circle {
    exch neg
    % .5 mul
    90 add
    dup cos 2 index mul
    3 1 roll
    sin mul
} bind def

/circmove{
    2 index { newpath } if
    circle moveto
    pop false
} bind def

/circline { circle lineto } bind def

/circcurve {
    6 2 roll circle
    6 2 roll circle
    6 2 roll circle
    curveto
} bind def

/circit { true { circmove } { circline } { circcurve } { closepath } pathforall pop } bind def
%
end
% END pst-text.pro
