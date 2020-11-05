' SpriteEditor
' Const and Vars

' init pgm
option explicit
option default none
option base 0

' 800x600
mode 1,8
cls
page write 0

' activate debugging output
const DEBUG = 0

' ----------------------------------------
const mul.x = mm.info(fontwidth)
const mul.y = mm.info(fontheight)

const cols  = MM.HRES/mul.x
const lines = MM.VRES/mul.y

dim integer keys(6)
dim integer taste
dim integer k_caps, k_num, k_scroll

' keycodes
'
' preferred syntax:
' CONST INTEGER K_ALT = 512 :-(
' I don't like % as modifier

CONST K_LALT%   = 1
CONST K_LCTRL%  = 2
const K_LGUI%   = 4
CONST K_LSHIFT% = 8
CONST K_RALT%   = 16
CONST K_RCTRL%  = 32
const K_RGUI%   = 64
CONST K_RSHIFT% = 128

CONST K_CTRL%  = 256
CONST K_ALT%   = 512
CONST K_SHIFT% = 1024
const K_GUI%   = 2048

const K_Enter          =  10
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
CONST K_ShiftCrsrDown  = 161+K_SHIFT   '! keycode
CONST K_ShiftCrsrLeft  = 130+K_SHIFT
CONST K_ShiftCrsrRight = 163+K_SHIFT   '! keycode

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

const K_TAB = 9
const K_ESC = 27
const K_SPC = 32

CONST K_F = 102
const K_G = 103
const K_L = 108
const K_P = 112
CONST K_R = 114
const K_S = 115
const K_T = 116
CONST K_X = 120
CONST K_Y = 121
const K_ShiftR = 82+K_SHIFT

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



' ----------------------------------------
' Dotsize
const PIX = 8

' Editor max dots x/y 
const SP.MAXX = 32
const SP.MAXY = 32

' Cursor Pos
dim integer CUR.X
dim integer CUR.Y

' index of current colour
dim integer FARBE = 1

' colour of empty/transparent pixels
const EMPTY = rgb(black)

' Colour Definition
dim integer MAX.colour = 7
dim integer farb(MAX.colour)=(rgb(black),rgb(blue),rgb(green),rgb(cyan),rgb(red),rgb(magenta),rgb(yellow),rgb(white))

' feature: implement an editor to change the colour

' ----------------------------------------
' Screen layout. Definition of different frames
'
' Sprite Editor
' Library
' Info/Help
' Colour Palette
' Preview

' ----------------------------------------
' Editor

const ED.X = 2
const ED.Y = 2
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
const INFO.x = 2
const INFO.y = 380
const INFO.w = 240
const INFO.h = 216

' ----------------------------------------
' Status Box
const STATUS.X = 600
const STATUS.Y = 2
const STATUS.w = 180
const STATUS.h = 200
dim string  STATUS.s = ""

' ----------------------------------------
' Colour Palette Box
const PAL.X = ED.X + PIX * SP.MAXX + 16            ' 32 * 8 + some distance
'dim integer PAL.X = 320
const PAL.Y = ED.y + PRE.H + 16
const PAL.w = 8*32+4
const PAL.h = 3*32+4
const PAL.grid = 32

' ----------------------------------------
' Libray

' sprites are stored on graphicspage 1
' load/save can easily managed just be save/load of graphics screen.
' no huge logic required.
const LIB.rows    = 8                         '  8r = 256/32
const LIB.columns = 16                        ' 16c = 512/32
const LIB.W       = LIB.columns * SP.MAXX
const LIB.H       = LIB.rows * SP.MAXY        ' =  8 * SP.MAXY
const LIB.X       = MM.HRES - LIB.W -4        ' Position Lib auf Bildschirm
const LIB.Y       = MM.VRES - LIB.H -4
const LIB.MAX     = LIB.rows * LIB.columns    ' max Sprites in Library
' feature: lib display ist nur eine page der gesamten Libray.
' mapping der Auswahl. 800x600 erlaubt mehr Platz

const LIB.PX = 0                              ' start mapping pos of sprites                     
const LIB.PY = 0
dim string  LIB.FNAME = "SPRITE-LIB.BMP"      ' Filename der Library
DIM INTEGER LIB.CUR   = 0                     ' current Lib position/sprite in editor

' coordinates of sprites save on page 1
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
' Sprite Memory
DIM INTEGER SP(SP.MAXX,SP.MAXY)

' ----------------------------------------
' where is the cursor, program state
const State.unknown = 0
const State.Editor  = 1
const State.Colour  = 2
const State.Library = 3
const State.exit    = 4
dim integer ED.State


'                                                                                             common
'----------------------------------------
' handle debug output
sub doDebug(s$)
  if DEBUG then
    open "debug.txt" for append as #1
    print #1,s$
    print #1,"keydown0 =";keydown(0)
    print #1,"keydown1 =";keydown(1)
    close #1
  endif
end sub

'----------------------------------------
' cursor position col/lines -> graphics pixels pos
sub doLocate(x as integer,y as integer)
  if (x <= 0)     then x=0
  if (y <= 0)     then y=0
  if (x >= cols)  then x=cols-1
  if (y >= lines) then y=lines-1
  print @(x * mul.x, y * mul.y) "";
end sub

'----------------------------------------
' doGetKey: returns the keycode 
' and set special key flags
sub doGetKey
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


'                                                                                            Library
' ----------------------------------------
sub doRefreshLibStatus
local integer x,y
local string s

  s = "Library: " + LIB.FNAME +" :"
  s = s + "  X " + STR$(LIB.cur mod LIB.columns)
  s = s + "  Y " + str$(int(LIB.cur/LIB.columns)) 
  s = s + "  cur " + str$(Lib.cur) + "   "
  Text LIBSTAT.X,LIBSTAT.Y+LIBSTAT.H/2, s, "LMN",1
  
end sub

' ----------------------------------------
sub doDrawLibStatus
' show status of library

  box LIBSTAT.X -1, LIBSTAT.Y -1, LIBSTAT.W +2, LIBSTAT.H +2, 1, rgb(white)
  doRefreshLibStatus
  
end sub


' ----------------------------------------
sub doRefreshLibrary
' load sprite data from graphics page 1
' and draw the to the library view
local integer x,y     ' sprite pos on editor screen
local integer x1,y1   ' sprite pos on page 1
local integer i

  doDebug("Func: doRefreshLibrary")

  for i=0 to LIB.MAX -1
    x = (i MOD LIB.columns) * SP.MAXX
    y = int(i / LIB.columns) * SP.MAXY
    x1 = LIB.px + x
    y1 = LIB.py + y
    x = LIB.X + x
    y = LIB.Y + y
    blit x1,y1,x,y,SP.MAXX,SP.MAXY,1
  next i

  ' mark current sprite
  x = LIB.X + (LIB.cur MOD LIB.columns) * SP.MAXX
  y = LIB.Y + int(LIB.cur / LIB.columns) * SP.MAXY

  if ED.State = State.Library then
    box x,y,SP.MAXX,SP.MAXY,1,rgb(white)
  else
    box x,y,SP.MAXX,SP.MAXY,1,rgb(gray)
  endif

end sub

' ----------------------------------------
sub doDrawLibrary

  doDebug("Func: doDrawLibrary")

  box LIB.X -1, LIB.Y -1, LIB.W +2, LIB.H +2, 1, RGB(WHITE)
  doRefreshLibrary
    
end sub

' ----------------------------------------
SUB doLibCursorMove (dx as integer, dy as integer)
local integer nx,ny

  doDebug("doLibCursorMove "+str$(dx)+","+str$(dy))

  nx = LIB.cur MOd LIB.columns
  ny = int(LIB.cur / LIB.columns)

  ' take care of the limits
  if (nx + dx) >= LIB.columns then dx=0
  if (ny + dy) >= LIB.rows    THEn dy=0
  nx = nx + dx
  ny = ny + dy  
  if (nx < 0) then nx = 0
  if (ny < 0) then ny = 0

  LIB.cur = nx + LIB.columns * ny

END SUB


' ----------------------------------------
sub doSaveLibrary

  doDebug("Func: doSaveLibrary")

  ' avoid keydown() bug(?)
  pause 200

  page write 2
  blit 0,0, 0,0, MM.HRES, MM.VRES, 0
  page write 0
  blit 0,0, 0,0, MM.HRES, MM.VRES, 1
  save image LIB.Fname, 0,0, LIB.w,LIB.h
  blit 0,0, 0,0, MM.HRES, MM.VRES, 2
  
end sub

' ----------------------------------------
sub doLoadLibrary
  page write 1
  load bmp LIB.Fname
  page write 0
end sub


' ----------------------------------------
sub doLibrary

local integer ch
local integer k

  doDebug("Func: doLibrary")
  
  doGetKey
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
    case K_ESC
      ED.state = State.exit
    case K_TAB
      ED.state = State.Editor
    case K_S
      doSaveLibrary
    case K_L
      doLoadLibrary
' ---------------------------------------- ctrl
' ---------------------------------------- shift
' ---------------------------------------- alt
  end select

  doRefreshLibrary
  doRefreshLibStatus
  
  if (ED.state <> State.Library) then
    doLibCursorMove(0,0)   ' clear cursor
  endif

end sub


'                                                                                             Editor
' ----------------------------------------
sub doDrawDot(sx as integer,sy as integer,c as integer)
local integer x,y

  x = ED.X + PIX * sx
  y = ED.Y + PIX * sy
  box  x, y, PIX, PIX, 1, c, c
  line x+PIX-1, y      , x+PIX-1, y+PIX-1, 1,rgb(gray)
  line x      , y+PIX-1, x+PIX-1, y+PIX-1, 1,rgb(gray) 

end sub


' ----------------------------------------
sub doDrawCursor
local integer x,y

  x = CUR.X * PIX + ED.X
  y = CUR.Y * PIX + ED.Y
  box  x, y, PIX, PIX, 1, rgb(white)
  line x + 1, y + 1,       x + PIX - 2, y + PIX - 2, 1,rgb(gray)
  line x + 1, y + PIX - 2, x + PIX - 2, y + 1,       1,rgb(gray) 

end sub

' ----------------------------------------
' move cursor by dx/dy
' common routine for all cursor moves

SUB doCursorMove (dx as integer, dy as integer)
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
' renew preview image
sub doRefreshPreview
local integer x,y, x1,y1

  doDebug("Func: doRefreshPreview")
  
  for x=0 to SP.MAXX-1
    for y=0 to SP.MAXY-1
      pixel PRE.x + x, PRE.Y + y, SP(x,y)
    next y
  next x
end sub


' ----------------------------------------
' read Sprite from preview
sub doReadFromPreview
local integer x,y

  doDebug("Func: doReadFromPreview")

for x=0 to SP.MAXX -1
  for y=0 to SP.MAXY -1
    SP(x,y) = PIXEL(PRE.X + x, PRE.Y +y)
  next y
next x

end sub


' ----------------------------------------
sub doDrawPreview

  doDebug("Func: doDrawPreview")

  box PRE.X -1, PRE.Y -1, PRE.W +2, PRE.H +2, 1, rgb(white)
  doRefreshPreview
  
end sub



' ----------------------------------------
sub doDrawEditor
local integer x,y

  doDebug("Func: doDrawEditor")
  
  box ED.X-1, ED.Y-1, PIX * SP.MAXX + 2, PIX * SP.MAXY + 2,1,rgb(WHITE)

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

  ' Sprite wird links mir rechter Spalte oder leer gefuellt
  if wrap then
    for y=0 to SP.MAXY -1
      vec(y) = SP(SP.MAXX-1, y)
    next y
  else
    for y=0 to SP.MAXY - 1
      vec(y) = EMPTY
    next y
  endif

  ' verschieben
  for y=0 to SP.MAXY -1
    for x=SP.MAXX-1 to 1 step -1
      SP(x,y) = SP(x-1,y)
    next x
  next y

  ' und auffuellen
  for y=0 to SP.MAXY - 1
    SP(0,y) = vec(y)
  next y

END SUB


' ----------------------------------------
SUB doSprite_ShiftLeft (wrap as integer)
local integer vec(SP.MAXY)
local integer x,y


  ' Sprite wird rechts mit linker Spalte oder leer gefuellt
  if wrap then
    for y=0 to SP.MAXY - 1
      vec(y) = SP(0, y)
    next y
  else
    for y=0 to SP.MAXY - 1
      vec(y) = EMPTY
    next y
  endif

  ' verschieben
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

  ' untere Sprite Zeile merken oder leer fuellen
  if wrap then
    for x=0 to SP.MAXX - 1
      vec(x) = SP(x, SP.MAXY-1)
    next x
  else
    for x=0 to SP.MAXX - 1
      vec(x) = EMPTY
    next x
  endif

  ' verschieben
  for x=0 to SP.MAXX - 1
    for y=SP.MAXY - 1 TO 1 step -1
      SP(x,y) = SP(x,y-1)
    next y
  next x

  ' und auffuellen
  for x=0 to SP.MAXX - 1
    SP(x,0) = vec(x)
  next x

END SUB

' ----------------------------------------
SUB doSprite_ShiftUp (wrap as integer)
local integer vec(SP.MAXX)
local integer x,y


  ' Sprite Zeile merken oder leer
  if wrap then
    for x=0 to SP.MAXX -1
      vec(x) = SP(x,0)
    next x
  else
    for x=0 to SP.MAXX - 1
      vec(x) = EMPTY
    next x
  endif

  ' verschieben
  for x=0 to SP.MAXX -1
    for y=0 to SP.MAXY - 2
      SP(x,y) = SP(x,y+1)
    next y
  next x

  ' und auffuellen
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
' direct = 1  : counter clockwise

' ccw = 3 x cw  -> feature skipped

local INTEGER SP1(SP.MAXX,SP.MAXY)
local integer x,y

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
sub doSpritePut
' write sprite in editor to current position in library
' via preview
local integer x,y
local integer x1,y1

  doDebug("Func: doSpritePut")

  x = (LIB.cur MOD LIB.columns) * SP.MAXX
  y = int(LIB.cur / LIB.columns) * SP.MAXY
  x1 = LIB.px + x
  y1 = LIB.py + y

  doDebug("Func: doSpritePut - x="+str$(x)+" ,y="+str$(y)+", x1="+str$(x1)+", y1="+str$(y1))

  page write 1
  blit PRE.x,PRE.y,x1,y1,SP.MAXX,SP.MAXY,0
  page write 0

  doRefreshLibrary

end sub

' ----------------------------------------
sub doSpriteGet
' read from library into editor
' via preview
local integer x,y
local integer x1,y1

  doDebug("Func: doSpriteGet")

  x = (LIB.cur MOD LIB.columns) * SP.MAXX
  y = int(LIB.cur / LIB.columns) * SP.MAXY
  x1 = LIB.px + x
  y1 = LIB.py + y

  doDebug("Func: doSpriteGet - x="+str$(x)+" ,y="+str$(y)+", x1="+str$(x1)+", y1="+str$(y1))

  page write 0
  blit x1,y1,PRE.x,PRE.y,SP.MAXX,SP.MAXY,1

  doReadFromPreview
  doRefreshLibrary

end sub

' ----------------------------------------
sub doEditor
' ----------------------------------------
' editor loop
' read keyboard and execute functions

local integer ch
local integer k

  doDebug("Func: doEditor")
  
  doDrawCursor
  doGetKey

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
    case K_ESC
      ED.state = State.exit
    case K_TAB
      ED.state = State.colour
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
    case K_0 TO K_7
      farbe = taste - K_0
    case K_P
      doSpritePut
    case K_G
      doSpriteGet
    case K_S
      doSaveLibrary
    case K_L
      doLoadLibrary
' ---------------------------------------- ctrl
    case K_CtrlCrsrUp
      doCursorMove(0,-5)
    case K_CtrlCrsrDown
      doCursorMove(0,5)
    case K_CtrlCrsrLeft
      doCursorMove(-5,0)
    case K_CtrlCrsrRight
      doCursorMove(5,0)
' ---------------------------------------- shift
    case K_ShiftCrsrUp
      doSprite_ShiftUp(0)
    case K_ShiftCrsrDown
      doSprite_ShiftDown(0)
    case K_ShiftCrsrLeft
      doSprite_ShiftLeft(0)
    case K_ShiftCrsrRight
      doSprite_ShiftRight(0)
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
  endif

  doDebug("Func: doEditor End")
    
end sub


'                                                                                            Palette
' ----------------------------------------
sub doDrawFarbpalette
local integer i,x,y

  doDebug("Func: doDrawFarbpalette")

  box PAL.x-1, PAL.y-1, PAL.w +2, PAL.h +2, 1, rgb(white)

  for i=0 to MAX.Colour
    x = PAL.x +i * PAL.grid ' + 1
    y = PAL.y ' + 2
    box x, y, PAL.grid, PAL.grid, 4, farb(i))
    print @(x+6, y+6) str$(i)
  next i
  
end sub


' ----------------------------------------
sub doFarbpalette
  ED.state = State.Library
end sub


'                                                                                          Help/Info
' ----------------------------------------
sub doDrawInfoHelp
local integer x,y
local string crsr$ = chr$(146)+chr$(147)+chr$(148)+chr$(149)

  box INFO.x -1, INFO.y -1, INFO.w +2, INFO.h +2,1,rgb(white)

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

  doLocate(x,y):         : print "1..8"
  doLocate(x+10,y): y=y+1: print "select colour"

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
sub doDrawStatus (s$)
local integer x,y
local string crsr$ = chr$(146)+chr$(147)+chr$(148)+chr$(149)

  doDebug("Func: doDrawStatus")
  
  box STATUS.x -1, STATUS.y -1, STATUS.w +2, STATUS.h +2,1,rgb(white)

  x = (STATUS.x / mul.x) '+ 1
  y = (STATUS.y / mul.y) '+ 1

  doLocate(x,y): y=y+1: print "      ";s$
  doLocate(x,y): y=y+1: print "Cur.X ";CUR.x;"  "
  doLocate(x,y): y=y+1: print "Cur.Y ";CUR.y;"  "
  doLocate(x,y): y=y+1: print "Keys  ";keydown(0);"  "
  doLocate(x,y): y=y+1: print "Key   ";keys(1);"  "
  doLocate(x,y): y=y+1: print "ctrl  ";k_ctrl;"  "
  doLocate(x,y): y=y+1: print "alt   ";k_alt;"  "
  doLocate(x,y): y=y+1: print "shift ";k_shift;"  "
  doLocate(x,y): y=y+1: print STATUS.s

end sub


' ----------------------------------------
sub doActivateEditor
end sub


' ----------------------------------------
SUB doDrawBildschirm
  doDrawEditor
  doDrawFarbpalette
  doDrawPreview
  doDrawLibrary
  doDrawLibStatus
  'doDrawNameTag
  doDrawinfoHelp
  doActivateEditor
END SUB


' ----------------------------------------
sub doInitVar
local integer x,y

  for x=0 to SP.MAXX
    for y=0 to SP.MAXY
      SP(x,y) = EMPTY
    next y
  next x

  ED.state = State.Editor
  CUR.x = 0
  CUR.y = 0
end sub

' ----------------------------------------
' main
' ----------------------------------------

doInitVar
doDrawBildschirm

do
  select case ED.state
    case State.Editor
      doEditor
    case State.Colour
      doFarbpalette
    case State.Library
      doLibrary
    case State.exit
      continue do
  end select
  doRefreshLibrary
loop

cls
end
