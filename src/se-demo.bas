' SpriteEditor Demo


#include "SPRITE-00.INC"

mode 2,12
cls
page write 1
restore SPRITE00DATA
for y=0 to 31
  for x=0 to 31
    read n
    pixel x,y,n 
  next x
next y
sprite read 1, 0,0,31,31,1
cls
page write 0
cls

load png "background.png"
image resize 0,0,320,200,0,0,640,400

do
for x=20 to 380
  sprite show 1, x,x,1
  pause 20
next x

for x=380 to 20 step -1
  sprite show 1, x,x,1
  pause 20 
next x
loop
end

