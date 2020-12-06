' SpriteEditor
' by BruderJo

' this software is public domain.
' copy, enjoy or delete it.
'
' 2020-12-06
'

' init pgm
option explicit
option default none
option base 0

' 640x480
' 8bit = 9 pages
' 12bit = 5 pages
mode 8,12


' Const and Vars
  
' activate debugging output = 1
const DEBUG = 0
  
' ----------------------------------------
const mul.x = mm.info(fontwidth)
const mul.y = mm.info(fontheight)

const cols  = MM.HRES\mul.x
const lines = MM.VRES\mul.y

dim integer keys(6)
dim integer taste
dim integer k_caps, k_num, k_scroll

' keycodes
CONST K_LALT   = 1
CONST K_LCTRL  = 2
const K_LGUI   = 4
CONST K_LSHIFT = 8
CONST K_RALT   = 16
CONST K_RCTRL  = 32
const K_RGUI   = 64
CONST K_RSHIFT = 128

CONST K_CTRL   = 256
CONST K_ALT    = 512
CONST K_SHIFT  = 1024
const K_GUI    = 2048
  
CONST K_CrsrUp         = 128
CONST K_CrsrDown       = 129
CONST K_CrsrLeft       = 130
CONST K_CrsrRight      = 131
CONST K_CtrlCrsrUp     = 128+K_CTRL
CONST K_CtrlCrsrDown   = 129+K_CTRL
CONST K_CtrlCrsrLeft   = 130+K_CTRL
CONST K_CtrlCrsrRight  = 131+K_CTRL
CONST K_AltCrsrUp      = 128+K_ALT
CONST K_AltCrsrDown    = 129+K_ALT
CONST K_AltCrsrLeft    = 130+K_ALT
CONST K_AltCrsrRight   = 131+K_ALT
CONST K_ShiftCrsrUp    = 128+K_SHIFT
CONST K_ShiftCrsrDown  = 161+K_SHIFT   '! keycode should be 129+K_SHIFT :-(
CONST K_ShiftCrsrLeft  = 130+K_SHIFT
CONST K_ShiftCrsrRight = 163+K_SHIFT   '! keycode should be 131+K_SHIFT :-(

CONST K_F1 = 145
CONST K_F2 = 146
CONST K_F3 = 147
CONST K_F4 = 148
CONST K_F5 = 149
CONST K_F6 = 150
CONST K_F7 = 151
CONST K_F8 = 152
CONST K_F9 = 153
CONST K_F10 = 154
CONST K_F11 = 155
CONST K_F12 = 156
  
const K_TAB   =  9
const K_Enter = 10
const K_ESC   = 27
const K_SPC   = 32
const K_PLUS  = 43
const K_MINUS = 45

const K_C =  99
CONST K_F = 102
const K_G = 103
const K_L = 108
const K_P = 112
CONST K_R = 114
const K_S = 115
const K_T = 116
CONST K_X = 120
CONST K_Y = 121
const K_ShiftC = 67 + K_SHIFT
const K_ShiftR = 82 + K_SHIFT

const K_0 = 48
const K_1 = 49
const K_2 = 50
const K_3 = 51
const K_4 = 52
const K_5 = 53
const K_6 = 54
const K_7 = 55
const K_8 = 56
const K_9 = 57
const K_Ctrl0 = 48 + K_CTRL
const K_Ctrl1 = 49 + K_CTRL
const K_Ctrl2 = 50 + K_CTRL
const K_Ctrl3 = 51 + K_CTRL
const K_Ctrl4 = 52 + K_CTRL
const K_Ctrl5 = 53 + K_CTRL
const K_Ctrl6 = 54 + K_CTRL
const K_Ctrl7 = 55 + K_CTRL
const K_Ctrl8 = 56 + K_CTRL
const K_Ctrl9 = 57 + K_CTRL


' ----------------------------------------
' Graphic Pages
'
' display     = 0 & 1
const PG_DISP = 0   ' main display, all graphics cmd write here. copy to page 0 to show

' ----------------------------------------
' Dotsize
const PIX = 6

' Editor max dots x/y
const SP.MAXX = 32
const SP.MAXY = 32

' Cursor Pos
dim integer CUR.X
dim integer CUR.Y

' index of current colour
dim integer FARBE = 1


' define some colours
const BOX.COLOUR   = rgb(254,254,254,15)     ' white
const CURS.COLOUR  = rgb(&H50,&H50,&H50,15)  ' gray
const PAL.SEL1     = rgb(&H50,&H50,&H50,15)  ' palette: mark colour1
const PAL.SEL2     = rgb(254,254,254,15)     ' palette: mark colour1
const LIBACTIVE.COLOUR = BOX.COLOUR
const LIBINACTIVE.COLOUR = CURS.COLOUR

' colour of empty/transparent pixels (black, fully transparent)
const EMPTY = rgb(0,0,0,0)


' Colour Definition
dim integer MAX.colour = 15
dim integer farb(MAX.colour)

' index to current background colour
dim integer ScreenColour = 0

' Color set (C64 shiny)
const C_00 = rgb(&H00,&H00,&H00,15)  ' black
const C_01 = rgb(&HFC,&HFC,&HFC,15)  ' white
const C_02 = rgb(&H8B,&H1F,&H00,15)  ' red
const C_03 = rgb(&H65,&HCD,&HA8,15)  ' cyan
const C_04 = rgb(&HA7,&H3B,&H9F,15)  ' purple
const C_05 = rgb(&H4F,&HB3,&H17,15)  ' green
const C_06 = rgb(&H1B,&H0D,&H93,15)  ' blue
const C_07 = rgb(&HF3,&HEB,&H5B,15)  ' yellow
const C_08 = rgb(&HA3,&H47,&H00,15)  ' orange
const C_09 = rgb(&H3F,&H1C,&H00,15)  ' brown
const C_0a = rgb(&HCB,&H7B,&H6F,15)  ' lt red
const C_0b = rgb(&H45,&H44,&H44,15)  ' grey1
const C_0c = rgb(&H83,&H83,&H83,15)  ' grey2
const C_0d = rgb(&H97,&HFF,&H97,15)  ' lt green
const C_0e = rgb(&H4F,&H93,&HD3,15)  ' lt blue
const C_0f = rgb(&HBB,&HBB,&HBB,15)  ' grey3

dim integer ColourTable(MAX.colour) = (C_00,C_01,C_02,C_03,C_04,C_05,C_06,C_07,C_08,C_09,C_0a,C_0b,C_0c,C_0d,C_0e,C_0f)


' feature planned: implement an editor to change the colour



' ----------------------------------------
' Screen layout. Definition of different frames
'
' ----------------------------------------
' Editor

const ED.X = 1
const ED.Y = 1
const ED.W = SP.MAXX * PIX
const ED.H = SP.MAXY * PIX

' ----------------------------------------
' Preview Fenster (right window)
const PRE.X = 320
const PRE.Y = 2
const PRE.W = SP.MAXX
const PRE.H = SP.MAXY
dim integer PRE.C = rgb(gray)  ' colour

' ----------------------------------------
' Info/Help Box (below editor)
const INFO.w = 240
const INFO.h = 236
const INFO.x = 2
const INFO.y = MM.VRES - 24 - INFO.h  ' or: ED.Y + ED.H + 4

' ----------------------------------------
' Colour Palette Box
const PAL.X = ED.X + ED.W + 16            ' 32 * 8 + some distance
const PAL.Y = ED.y + PRE.H + 16
const PAL.grid = 32
const PAL.w = ((Max.colour mod 8) + 1) * 32
const PAL.h = (Max.colour\8 + 1)* PAL.grid

' ----------------------------------------
' Libray

' sprites are stored on graphicspage PG_LIB
' load/save can easily managed just be save/load of graphics screen.
' no huge logic required.
const LIB.rows    = 8                         ' 8r = 256/32
const LIB.columns = 8                         ' 8c = 256/32
const LIB.W       = LIB.columns * SP.MAXX
const LIB.H       = LIB.rows * SP.MAXY        ' =  8 * SP.MAXY
const LIB.X       = MM.HRES - LIB.W -4        ' Position Lib on screen
const LIB.Y       = MM.VRES - LIB.H -4
const LIB.MAX     = LIB.rows * LIB.columns    ' max Sprites in Library

dim integer LIBSTORE (LIB.MAX, SP.MAXX, SP.MAXY)  ' all library sprite data
dim integer LIB.SPRITE.X (LIB.MAX)            ' x/y coordinates of sprites in library
dim integer LIB.SPRITE.Y (LIB.MAX)

const LIB.PX = 0                              ' start mapping pos of sprites
const LIB.PY = 0
dim string  LIB.FNAME = "SPRITE-LIB.DAT"      ' Filename of the Library
DIM INTEGER LIB.CUR   = 0                     ' current Lib position/choosen sprite in editor

' visibility of cursor in Library
const LIBCURS.NONE      = 0
CONST LIBCURS.ACTIVE    = 1
const LIBCURS.INACTIVE  = 2

' coordinates of sprites save on PG_LIB
DIM integer LIB.SPX (LIB.MAX)
DIM integer LIB.SPY (LIB.MAX)

' akt. Element der Library
dim integer LIB.pos = 0

' ----------------------------------------
' Libray Status
const LIBSTAT.X = LIB.X
const LIBSTAT.Y = LIB.Y - 34
const LIBSTAT.W = LIB.W
const LIBSTAT.H = 32

' ----------------------------------------
' Status and Error Box
const STATUS.X = ED.X + ED.W + 16
const STATUS.Y = PAL.Y + PAL.h + 8
const STATUS.W = PAL.w
const STATUS.H = 2 * PAL.grid
const STATUS.MaxLen = 40
dim string STATUS.msg1 = ""
dim string STATUS.msg2 = ""

' ----------------------------------------
' Sprite Memory
DIM INTEGER SP(SP.MAXX,SP.MAXY)

' ----------------------------------------
' where is the cursor, program state
const State.unknown = 0
const State.Editor  = 1
const State.Colour  = 2
const State.Library = 3
const State.Redraw  = 4
const State.exit    = 9
dim integer ED.State



'                                                                                             common
'----------------------------------------
sub doDebug(s$)
' handle debug output

  if DEBUG then
    open "debug.txt" for append as #1
    print #1,s$
    print #1,"keydown0 =";keydown(0)
    print #1,"keydown1 =";keydown(1)
    close #1
  endif
end sub
  
'----------------------------------------
sub doLocate(x as integer,y as integer)
' cursor position col/lines -> graphics pixels pos

  if (x <= 0)     then x=0
  if (y <= 0)     then y=0
  if (x >= cols)  then x=cols-1
  if (y >= lines) then y=lines-1
  print @(x * mul.x, y * mul.y) "";

end sub

  
'----------------------------------------
sub doGetKey
' doGetKey: returns the keycode
' and set special key flags

local integer kmod,klck
local integer j,k
local a$,b$
  
  doDebug("Func: doGetKey")
  
  ' wait until key pressed
  do
    a$ = inkey$
  loop until a$ <> ""
  
  k = keydown(1)
  
  kmod  = keydown(7)
  klck  = keydown(8)
  if ((kmod and K_LALT) or (kmod AND K_RALT)) then
    k = k + K_ALT
  elseif ((kmod and K_LCTRL) or (kmod AND K_RCTRL)) then
    k = k + K_CTRL
  elseif ((kmod and K_LSHIFT) or (kmod AND K_RSHIFT)) then
    k = k + K_SHIFT%
  elseif ((kmod and K_LGUI) or (kmod AND K_RGUI)) then
    k = k + K_GUI
  endif
  
  taste = k
  
  
  b$="":if (k>32) and (k<128) then b$=chr$(k)
  doDebug("Func: doGetKey, result =("+b$+") "+str$(k))
  
  
  'DO WHILE KEYDOWN(0) AND INKEY$ <> "" : LOOP
  DO WHILE inkey$ <> "" : LOOP
  
end sub
  
  
'                                                                                             Status
' ----------------------------------------
sub doRefreshStatusBox
local integer x,y

  x = STATUS.X + 4
  y = STATUS.Y + 4
  box STATUS.X, STATUS.Y, STATUS.w, STATUS.h, 1, farb(screencolour), farb(screencolour)
  Text x,y,    STATUS.msg1, "LMN",7
  TEXT x,y+12, STATUS.msg2, "LMN",7
end sub


sub doClearError

  STATUS.msg2 = ""
  doRefreshStatusBox
  
end sub

sub doError ( s as string)
local string s1

  if (s <> "") then
    s1 = left$(s,STATUS.MaxLen)
    if (s1 = STATUS.msg2) then     ' if old-status = new-status then no need to redraw
      exit sub
    endif
  endif
  STATUS.msg2 = s1
  
  doRefreshStatusBox

end sub


sub doStatus ( s as string)
local string s1

  if (s <> "") then
    s1 = left$(s,STATUS.MaxLen)
    if (s1 = STATUS.msg1) then     ' if old-status = new-status then no need to redraw
      exit sub
    endif
  endif
  STATUS.msg1 = s1

  doRefreshStatusBox

end sub


sub doStatusBox

  box STATUS.X - 1, STATUS.Y -1, STATUS.w + 2, STATUS.h + 2, 1, BOX.COLOUR
  doRefreshStatusBox
  
end sub


'                                                                                            Library
' ----------------------------------------
sub doRefreshLibStatus
local integer x,y
local string s1,s2
  
  s1 = "Library: " + LIB.FNAME
  s2 = "  X " + STR$(LIB.cur mod LIB.columns)
  s2 = s2 + "  Y " + str$(int(LIB.cur/LIB.columns))
  s2 = s2 + "  cur " + str$(Lib.cur) + "   "
  Text LIBSTAT.X,LIBSTAT.Y+LIBSTAT.H/3, s1, "LMN",7
  text LIBSTAT.X + LIBSTAT.W\2, LIBSTAT.Y + 2*LIBSTAT.H\3, s2, "LMN",7
  
end sub
  
' ----------------------------------------
sub doDrawLibStatus
' show status of library
  
  box LIBSTAT.X -1, LIBSTAT.Y -1, LIBSTAT.W +2, LIBSTAT.H +2, 1, BOX.COLOUR
  doRefreshLibStatus
  
end sub
  
' ----------------------------------------
sub doDrawLibCursor(m as integer)
' draw the library cursor
' m = 0 delete cursor
' m = 1 draw active cursor
' m = 2 draw inactive cursor

local integer x,y
local integer c

  x = LIB.X + (LIB.cur MOD LIB.columns) * SP.MAXX
  y = LIB.Y + (LIB.cur \ LIB.columns) * SP.MAXY

  select case m
    case LIBCURS.NONE
      c = farb(ScreenColour)
    case LIBCURS.ACTIVE
      c = LIBACTIVE.COLOUR
    case LIBCURS.INACTIVE
      c = LIBINACTIVE.COLOUR
  end select

  page write 1
  box x,y,SP.MAXX,SP.MAXY,1,c
  page write 0

end sub


' ----------------------------------------
sub doRefreshLibrary
' draw sprite data into the library view
local integer x,y     ' sprite pos
local integer i
  
  doDebug("Func: doRefreshLibrary")
  
  for i=0 to LIB.MAX-1
    x = LIB.SPRITE.x(i)
    y = LIB.SPRITE.y(i)
    sprite show i+1, x, y, 1
  next i

  ' draw frame around selected library item
  if ED.State = State.Library then
    doDrawLibCursor(LIBCURS.ACTIVE)
  else
    doDrawLibCursor(LIBCURS.INACTIVE)
  endif

end sub


' ----------------------------------------
sub doDrawLibrary

  doDebug("Func: doDrawLibrary")

  box LIB.X -1, LIB.Y -1, LIB.W +2, LIB.H +2, 1, BOX.COLOUR
  doRefreshLibrary

end sub


' ----------------------------------------
SUB doLibCursorMove (dx as integer, dy as integer)
local integer nx,ny

  doDebug("doLibCursorMove "+str$(dx)+","+str$(dy))

  nx = LIB.cur MOd LIB.columns
  ny = LIB.cur \ LIB.columns

  ' take care of the limits
  if (nx + dx) >= LIB.columns then dx=0
  if (ny + dy) >= LIB.rows    THEn dy=0
  nx = nx + dx
  ny = ny + dy
  if (nx < 0) then nx = 0
  if (ny < 0) then ny = 0

  ' delete old cursor position
  doDrawLibCursor(0)  
  LIB.cur = nx + LIB.columns * ny

  if ED.State = State.Library then
    doDrawLibCursor(LIBCURS.ACTIVE)
  else
    doDrawLibCursor(LIBCURS.INACTIVE)
  endif
  
END SUB


' ----------------------------------------
sub doSaveLibrary
local integer x,y
local integer i

  doDebug("Func: doSaveLibrary")

  doStatus("Save Library")
  
  ' avoid keydown() bug of 5.05.05
  pause 100

  open LIB.Fname for output as #4
  print #4,"CMM2-Sprite";chr$(1);            ' output identifier and version 
  print #4,chr$(SP.MAXX);chr$(SP.MAXY);      ' dimension x/y

  ' now output all sprite data
  for i=0 to LIB.MAX-1
    for x=0 to SP.MAXX-1
      for y=0 to SP.MAXY-1
        print #4, bin2str$(UINT64,LIBSTORE(i,x,y));
      next y
    next x
  next i
  close 4

end sub


' ----------------------------------------
sub doLoadLibrary
local integer x,y
local integer i
local string s

  doStatus("Load Library")
  
  open LIB.Fname for input as #5
  s = input$(11,#5)
  if (s <> "CMM2-Sprite") then
    doError("Sprite read error. Identifier mismatch.")
    close #5
    exit sub
  endif
  s = input$(1,#5)
  if (s <> chr$(1)) then
    doError("Sprite read error. Version mismatch.")
    close #5
    exit sub
  endif
  x = asc(input$(1,#5))
  y = asc(input$(1,#5))
  if ((x <> SP.MAXX) or (y <> SP.MAXY)) then
    doError("Sprite read error. Sprite size not 32x32.")
    close #5
    exit sub
  endif

  ' now output all sprite data
  for LIB.cur=0 to LIB.MAX-1
    doStatus("Load Library : "+str$(LIB.cur))
    for x=0 to SP.MAXX-1
      for y=0 to SP.MAXY-1
        s = input$(8,#5)
        SP(x,y) = str2bin(UINT64,s)
      next y
    next x
    doRefreshPreview
    doSpritePut
    doDrawLibCursor(0)
    
  next LIB.cur
  close 5

  LIB.cur = 0
  doSpriteGet
  
  
end sub
  
  
  
'                                                                                             Editor
' ----------------------------------------
sub doDrawDot(sx as integer,sy as integer,c as integer)
local integer x,y

  x = ED.X + PIX * sx
  y = ED.Y + PIX * sy

  page write 1
  box  x, y, PIX, PIX, 1, c, c
  page write 0

  line x+PIX-1, y      , x+PIX-1, y+PIX-1, 1,CURS.COLOUR
  line x      , y+PIX-1, x+PIX-1, y+PIX-1, 1,CURS.COLOUR

  page write PG_DISP

end sub


' ----------------------------------------
sub doDrawCursor
local integer x,y

  x = CUR.X * PIX + ED.X
  y = CUR.Y * PIX + ED.Y
  page write 1
  box  x, y, PIX, PIX, 1, BOX.COLOUR
  line x + 1, y + 1,       x + PIX - 2, y + PIX - 2, 1,CURS.COLOUR
  line x + 1, y + PIX - 2, x + PIX - 2, y + 1,       1,CURS.COLOUR
  page write 0

  x = PAL.X
  y = ED.y + PIX
  Text x,y, str$(CUR.x)+" ", "LMN",7
  text x+16,y, str$(CUR.y)+" ", "LMN",7

end sub


' ----------------------------------------
SUB doCursorMove (dx as integer, dy as integer)
' move cursor by dx/dy
' common routine for all cursor moves

local integer nx,ny

  doDebug("doCursorMove Start - CUR="+str$(CUR.x)+","+str$(CUR.y))

  doDrawDot(CUR.X, CUR.Y,SP(CUR.x, CUR.y))     ' clear cursor
  CUR.X = CUR.X + dx
  CUR.Y = CUR.Y + dy
  if (CUR.X < 0) then CUR.X = CUR.X + SP.MAXX  ' wrap around
  if (CUR.Y < 0) then CUR.Y = CUR.Y + SP.MAXY
  if (CUR.X >= SP.MAXX) then CUR.X = CUR.X - SP.MAXX
  if (CUR.Y >= SP.MAXY) then CUR.Y = CUR.Y - SP.MAXY
  
  ' dx=0 AND dy=0 -> clear cursor
  if (dx <> 0) or (dy <> 0) then
    doDrawCursor
  endif
  
  doDebug("doCursorMove End   - CUR="+str$(CUR.x)+","+str$(CUR.y))
  
  
END SUB

  
' ----------------------------------------
sub doRefreshPreview
' renew preview image

local integer x,y, x1,y1

  doDebug("Func: doRefreshPreview")

  page write 1

  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      pixel PRE.x + x, PRE.Y + y, SP(x,y)
    next y
  next x

  page write PG_DISP
  
end sub
  
  
' ----------------------------------------
' read Sprite from preview
sub doReadFromPreview
local integer x,y
  
  doDebug("Func: doReadFromPreview")
  
  ' early exit. data are already in SP() available
  exit sub
  
  for x=0 to SP.MAXX -1
    for y=0 to SP.MAXY -1
      SP(x,y) = PIXEL(PRE.X + x, PRE.Y +y)
    next y
  next x
  
end sub


' ----------------------------------------
sub doDrawPreview

  doDebug("Func: doDrawPreview")

  box PRE.X -1, PRE.Y -1, PRE.W +2, PRE.H +2, 1, BOX.COLOUR
  ' box PRE.x + SP.MAXX -1, PRE.y -1, SP.MAXX +2, SP.MAXY +2, 1, BOX.COLOUR
  doRefreshPreview

end sub



' ----------------------------------------
sub doDrawEditor
local integer x,y
  
  doDebug("Func: doDrawEditor")
  
  box ED.X-1, ED.Y-1, PIX * SP.MAXX + 2, PIX * SP.MAXY + 2,1, BOX.COLOUR
  
  for x=0 to SP.MAXX - 1
    for y=0 to SP.MAXY - 1
      doDrawDot(x, y, SP(x,y))
    next y
  next x
  
  doDrawCursor
  
end sub


' ----------------------------------------
' space key toggles dot colour (set/reset)
sub doToggleDot
  if SP(CUR.x, CUR.y) <> farb(farbe) then
    SP(CUR.x,CUR.y) = farb(farbe)
  else
    SP(CUR.x,CUR.y) = EMPTY
  endif
  
end sub


' ----------------------------------------
SUB doSprite_ShiftRight (wrap as integer)
local integer vec(SP.MAXY)
local integer x,y

  ' left row will become the right one or filled with EMPTY
  if wrap then
    for y=0 to SP.MAXY -1
      vec(y) = SP(SP.MAXX-1, y)
    next y
  else
    for y=0 to SP.MAXY - 1
      vec(y) = EMPTY
    next y
  endif

  ' move
  for y=0 to SP.MAXY -1
    for x=SP.MAXX-1 to 1 step -1
      SP(x,y) = SP(x-1,y)
    next x
  next y

  ' and fill
  for y=0 to SP.MAXY - 1
    SP(0,y) = vec(y)
  next y

END SUB


' ----------------------------------------
SUB doSprite_ShiftLeft (wrap as integer)
local integer vec(SP.MAXY)
local integer x,y


  ' fill Sprite with left col or empty col
  if wrap then
    for y=0 to SP.MAXY - 1
      vec(y) = SP(0, y)
    next y
  else
    for y=0 to SP.MAXY - 1
      vec(y) = EMPTY
    next y
  endif

  ' now move
  for y=0 to SP.MAXY - 1
    for x=0 TO SP.MAXX - 2
      SP(x,y) = SP(x+1,y)
    next x
  next y

  ' und auffuellen
  for y=0 to SP.MAXY - 1
    SP(SP.MAXX-1,y) = vec(y)
  next y

END SUB


' ----------------------------------------
SUB doSprite_ShiftDown (wrap as integer)
local integer vec(SP.MAXX)
local integer x,y

  ' remember lower row or fill with EMPTY
  if wrap then
    for x=0 to SP.MAXX - 1
      vec(x) = SP(x, SP.MAXY-1)
    next x
  else
    for x=0 to SP.MAXX - 1
      vec(x) = EMPTY
    next x
  endif

  ' move
  for x=0 to SP.MAXX - 1
    for y=SP.MAXY - 1 TO 1 step -1
      SP(x,y) = SP(x,y-1)
    next y
  next x

  ' and fill
  for x=0 to SP.MAXX - 1
    SP(x,0) = vec(x)
  next x

END SUB

' ----------------------------------------
SUB doSprite_ShiftUp (wrap as integer)
local integer vec(SP.MAXX)
local integer x,y
  
  
  ' remember row or delete it
  if wrap then
    for x=0 to SP.MAXX -1
      vec(x) = SP(x,0)
    next x
  else
    for x=0 to SP.MAXX - 1
      vec(x) = EMPTY
    next x
  endif

  ' move
  for x=0 to SP.MAXX -1
    for y=0 to SP.MAXY - 2
      SP(x,y) = SP(x,y+1)
    next y
  next x

  ' and fill
  for x=0 to SP.MAXX - 1
    SP(x,SP.MAXY-1) = vec(x)
  next x

END SUB


' ----------------------------------------
sub doSpriteFill
' fill sprite with selected colour

local integer x,y
  
  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY -1
      SP(x,y) = farb(farbe)
    next y
  next x
  
end sub


' ----------------------------------------
sub doSpriteMirrorX
local INTEGER SP1(SP.MAXX,SP.MAXY)
local integer x,y

  ' temp copy of sprite
  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      SP1(x,y) = SP(x,y)
    next y
  next x

  for y=0 to SP.MAXY-1
    for x=0 to SP.MAXX-1
      SP(x,y) = SP1(SP.MAXX-1-x,y)
    next x
  next y

end sub


' ----------------------------------------
sub doSpriteMirrorY
local INTEGER SP1(SP.MAXX,SP.MAXY)
local integer x,y

  ' temp copy of sprite
  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      SP1(x,y) = SP(x,y)
    next y
  next x
  
  for x=0 to SP.MAXY-1
    for y=0 to SP.MAXX-1
      SP(x,y) = SP1(x,SP.MAXY-1-y)
    next y
  next x
  
end sub
  
  
' ----------------------------------------
sub doSpriteRotate(direct as integer)
' rotate sprite
' direct = 0  : clockwise
' direct = 1  : counter clockwise - not implemented. 3 x 'R' is the same

local INTEGER SP1(SP.MAXX,SP.MAXY)
local integer x,y

  ' programmer's hint. 3x3 matrix before & after
  '         ->    <-
  '  123   741   369
  '  456   852   258
  '  789   963   147

  ' rotate only, if sprite is a square
  if SP.MAXX <> SP.MAXY then
    exit sub
  endif

  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      SP1(x,y) = SP(x,y)
    next y
  next x

  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      SP(x,y) = SP1(y,SP.MAXY-1-x)
    next y
  next x

end sub


' ----------------------------------------
sub doSpriteClear
local integer x,y

  ' clear current sprite in editor
  for x=0 to SP.MAXX
    for y=0 to SP.MAXY
      SP(x,y) = EMPTY
    next y
  next x

end sub


' ----------------------------------------
sub doSpritePut
' write sprite in editor to current position in library
' via preview

local integer x,y
local integer i

  doDebug("Func: doSpritePut")

  x = LIB.SPRITE.X(LIB.cur)
  y = LIB.SPRITE.Y(LIB.cur)
  

  ' LIBSTORE(LIB.cur, SP(*,*))
  ' refresh sprite definition with current preview
  i = LIB.cur + 1
  sprite read i, PRE.x,PRE.y, SP.MAXX, SP.MAXY, 1
  sprite show i, x, y, 1

  ' copy sprite date to library
  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      LIBSTORE(LIB.cur, x,y) = SP(x,y)
    next y
  next x

  doRefreshLibrary

end sub

  
' ----------------------------------------
sub doSpriteGet
' read from library into editor
' via preview

local integer x,y

  doDebug("Func: doSpriteGet")

  ' copy sprite date from library
  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      SP(x,y) = LIBSTORE(LIB.cur, x,y)
    next y
  next x

  doRefreshLibrary

end sub
  
' ----------------------------------------
sub doScreenColour(d as integer)
' change screen background colour
  
  ScreenColour = ScreenColour + d
  if ScreenColour < 0 then ScreenColour = 0
  if ScreenColour > Max.Colour then ScreenColour = Max.Colour
  
end sub
  
  
  
'                                                                                            Palette
' ----------------------------------------
sub doRefreshPalette
local integer i,x,y
  
  doDebug("Func: doRefreshPalette")
  
  for i=0 to MAX.Colour
    x = PAL.x + (i mod 8) * PAL.grid
    y = PAL.y + (i > 7) * PAL.grid
    box x, y, PAL.grid, PAL.grid, 1, rgb(black), farb(i)
    print @(x+6, y+6) str$(i)
  next i
  
  x = PAL.x + (farbe MOD 8) * PAL.grid
  y = PAL.y + (farbe > 7) * PAL.grid
  x = x + PAL.grid/2
  y = y + PAL.grid/2
  circle x, y, PAL.grid/2 - 1, 1, 1, PAL.SEL1
  circle x, y, PAL.grid/2 - 2, 1, 1, PAL.SEL2
  
end sub
  
  
' ----------------------------------------
sub doDrawFarbpalette
  
  doDebug("Func: doDrawFarbpalette")
  
  box PAL.x-1, PAL.y-1, PAL.w +2, PAL.h +2, 1, PAL.SEL1
  box PAL.x,   PAL.y  , PAL.w,    PAL.h,    0, PAL.SEL2, PAL.SEL2
  doRefreshPalette
  
end sub
  
  
' ----------------------------------------
sub doFarbpalette
  
  ' do nothing. jump to next activity window
  ED.state = State.Library
end sub
  
  
'                                                                                          Help/Info
' ----------------------------------------
sub doDrawInfoHelp
local integer x,y
local string crsr$ = chr$(146)+chr$(147)+chr$(148)+chr$(149)  ' cursor chars
  
  box INFO.x -1, INFO.y -1, INFO.w +2, INFO.h +2,1, BOX.COLOUR
  
  x = (INFO.x / mul.x) + 1
  y = (INFO.y / mul.y) + 1
  
  font 7
  doLocate(x,y):         : print "space"
  doLocate(x+10,y): y=y+1: print "paint/delete dot"
  
  doLocate(x,y):         : print "     ";:font 1:print crsr$;:font 7
  doLocate(x+10,y): y=y+1:print "move cursor 1 dot"
  
  doLocate(x,y):         : print "ctrl ";:font 1: print crsr$;:font 7
  doLocate(x+10,y): y=y+1: print "move cursor 5 dots"
  
  doLocate(x,y):         : print "shft ";:font 1: print crsr$;:font 7
  doLocate(x+10,y): y=y+1: print "shift sprite"
  
  doLocate(x,y):         : print "alt  ";:font 1: print crsr$;:font 7
  doLocate(x+10,y): y=y+1: print "scroll sprite"
  
  doLocate(x,y):         : print "F"
  doLocate(x+10,y): y=y+1: print "fill with current colour"
  
  doLocate(x,y):         : print "R"
  doLocate(x+10,y): y=y+1: print "rotate clockwise"
  
  'doLocate(x,y): y=y+1: print "shft R     rotate counterclockwise"
  
  doLocate(x,y):         : print "X"
  doLocate(x+10,y): y=y+1: print "mirror vertically"
  
  doLocate(x,y):         : print "Y"
  doLocate(x+10,y): y=y+1: print "mirror horizontally"
  
  doLocate(x,y):         : print "0..9"
  doLocate(x+10,y): y=y+1: print "select colour 0-9"
  doLocate(x,y):         : print "ctrl 0..5"
  doLocate(x+10,y): y=y+1: print "select colour 10-15"
  
  'doLocate(x,y): y=y+1: print "T          toggle preview bkgrd colour"
  
  doLocate(x,y):         : print "tab"
  doLocate(x+10,y): y=y+1: print "toggle active window"
  
  doLocate(x,y):         : print "P"
  doLocate(x+10,y): y=y+1: print "put sprite into library"
  
  doLocate(x,y):         : print "G"
  doLocate(x+10,y): y=y+1: print "get sprite from library"
  
  doLocate(x,y):         : print "S or F4"
  doLocate(x+10,y): y=y+1: print "save library"
  
  doLocate(x,y):         : print "L or F5"
  doLocate(x+10,y): y=y+1: print "load library"
  
  doLocate(x,y):    y=y+1: print ""
  ' F6/F7  export/import sprite as PNG
  font 1
  
end sub
  
  
  
' ----------------------------------------
Sub doKeyboardHandler
' ----------------------------------------
' keyboard loop
' read keyboard and execute functions

local integer ch
local integer k
  
  doDebug("Func: doKeyboardHandler")
  
  doDrawCursor
  doGetKey
  
  ' common functions
  select case taste
    case K_ESC
      ED.state = State.exit
    case K_P
      doSpritePut
    case K_G
      doSpriteGet
    case K_S, K_F4
      doSaveLibrary
      ED.state = State.Redraw
    case K_L, K_F5
      doLoadLibrary
  end select
  
  '
  if ED.state = State.Editor then
    
    select case taste
        ' ---------------------------------------- normal keys
      case K_CrsrUp
        doCursorMove(0,-1)
      case K_CrsrDown
        doCursorMove(0,1)
      case K_CrsrLeft
        doCursorMove(-1,0)
      case K_CrsrRight
        doCursorMove(1,0)
      case K_TAB
        ED.state = State.Library  ' planned: State.Colour
      case K_SPC
        doToggleDot
      case K_F
        doSpriteFill
      case K_R
        doSpriteRotate(1)
      case K_X
        doSpriteMirrorX
      case K_Y
        doSpriteMirrorY
      case K_0 TO K_9
        farbe = taste - K_0
        doRefreshPalette
        ' temp. inofficial function - test sprite background
      case K_PLUS
        doScreenColour(1)
        ED.state = State.Redraw
      case K_MINUS
        doScreenColour(-1)
        ED.state = State.Redraw
        ' ---------------------------------------- ctrl
      case K_CtrlCrsrUp
        doCursorMove(0,-5)
      case K_CtrlCrsrDown
        doCursorMove(0,5)
      case K_CtrlCrsrLeft
        doCursorMove(-5,0)
      case K_CtrlCrsrRight
        doCursorMove(5,0)
      case K_Ctrl0 to K_Ctrl5
        farbe = 10 + taste - K_0 - K_Ctrl
        doRefreshPalette
        ' ---------------------------------------- shift
      case K_ShiftCrsrUp
        doSprite_ShiftUp(0)
      case K_ShiftCrsrDown
        doSprite_ShiftDown(0)
      case K_ShiftCrsrLeft
        doSprite_ShiftLeft(0)
      case K_ShiftCrsrRight
        doSprite_ShiftRight(0)
      case K_ShiftC
        doSpriteClear
        'case K_ShiftR
        '  doSpriteRotate(-1)
        ' ---------------------------------------- alt
      case K_AltCrsrUp
        doSprite_ShiftUp(1)
      case K_AltCrsrDown
        doSprite_ShiftDown(1)
      case K_AltCrsrLeft
        doSprite_ShiftLeft(1)
      case K_AltCrsrRight
        doSprite_ShiftRight(1)
    end select
    
    doRefreshPreview
    
    doDrawEditor
    
    if (ED.state <> State.Editor) then
      doCursorMove(0,0)   ' clear cursor
      doLibCursorMove(0,0)
    endif
    
  elseif ED.state = State.Library then
    
    select case taste
        ' ---------------------------------------- normal keys
      case K_CrsrUp
        doLibCursorMove(0,-1)
      case K_CrsrDown
        doLibCursorMove(0,1)
      case K_CrsrLeft
        doLibCursorMove(-1,0)
      case K_CrsrRight
        doLibCursorMove(1,0)
      case K_TAB
        ED.state = State.Editor
        ' ---------------------------------------- ctrl
        ' ---------------------------------------- shift
        ' ---------------------------------------- alt
    end select
    
    doRefreshLibrary
    doRefreshLibStatus
    
    if (ED.state <> State.Library) then
      doLibCursorMove(0,0)   ' clear cursor
    endif
    
  endif
  
  doDebug("Func: doKeyboardHandler End")
  
  
end sub
  
  
  
' ----------------------------------------
SUB doDrawBildschirm

  colour rgb(white),farb(ScreenColour)
  cls
  
  doDrawEditor
  doDrawFarbpalette
  doDrawPreview
  doDrawLibrary
  doDrawLibStatus
  'doDrawNameTag
  doDrawinfoHelp
  doStatusBox
  
END SUB


' ----------------------------------------
sub doInitSpriteLib
' write sprite in editor to current position in library
' via preview

local integer x,y
local integer x1,y1
local integer i

  doDebug("Func: doSpriteLibInit")
  
  x = (LIB.cur MOD LIB.columns) * SP.MAXX
  y = (LIB.cur \ LIB.columns) * SP.MAXY
  x1 = LIB.X + x
  y1 = LIB.Y + y
  
  ' refresh sprite definition with current preview
  LIB.SPRITE.x(LIB.cur) = x1
  LIB.SPRITE.y(LIB.cur) = y1
  i = LIB.cur + 1
  sprite read i, PRE.x,PRE.y, SP.MAXX, SP.MAXY, 1
  sprite show 1, x1, y1, 1

  ' copy sprite date to library
  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      LIBSTORE(LIB.cur, x,y) = EMPTY
    next y
  next x
  
end sub


' ----------------------------------------
sub doInitVar
local integer x,i

  print "INIT VARs"
  ' load active colours with colour table
  for x=0 to MAX.colour
    farb(x) = ColourTable(x)
  next x
  
  doSpriteClear
  
  ' init sprite data in libray
  doDrawPreview
  for LIB.cur=0 to LIB.MAX - 1
    doInitSpriteLib
    print ".";
    if (LIB.cur MOD 10)=0 then print
  next LIB.cur
  
  LIB.cur = 0
  ' current program state and cursor position
  ED.state = State.Editor
  CUR.x = 0
  CUR.y = 0
end sub
  
  
' ----------------------------------------
' main
' ----------------------------------------
  
' initially clear all screens
page write PG_DISP
cls

doInitVar
doDrawBildschirm


do
  select case ED.state
    case State.Editor
      doStatus("Editor")
      doKeyboardHandler
    case State.Colour
      doFarbpalette
    case State.Library
      doStatus("Library")
      doKeyboardHandler
    case State.Redraw
      doDrawBildschirm
      ED.state = State.Editor
    case State.exit
      continue do
  end select
  ' doRefreshLibrary
loop

cls
end
