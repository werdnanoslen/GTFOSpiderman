 rem ===========SETUP==========
 set legacy 0.99
 set optimization inlinerand
 const pfscore=1
 rem ===========/SETUP=========


 rem ===========VARS===========
 dim sc1 = score
 dim sc2 = score+1
 dim sc3 = score+2

 dim p0posx=a
 dim p1posx=b

 dim p0power=c.d

 dim ballflies=e
 dim pface=f

 dim gravity=g.h

 dim ballposy=i.j

 dim random=k
 dim lethal=l
 dim count=m
 rem ===========/VARS==========


 rem ===========INIT===========
 rem players' initial x pos
 p0posx=40
 p1posx=100

 rem players' initial y pos
 player0y=31
 player1y=70

 rem players' initial power/score
 p0power=0.0
 pfscore2=21
 score=0

 rem ball initial placement
 ballflies=0
 pface=0
 gravity=0.0
 ballposy=0.0
 lethal=0
 count=10
 rem ===========/INIT==========


 rem ===========DRAW===========
titlecard
 COLUBK=$40
 COLUPF=$70

 drawscreen
 drawscreen
 drawscreen
 
 COLUBK=$70
 COLUPF=$40
 playfield:
 .XXXX..XXXX..XXX.XXX...XXX.XXX..
 X......X...X..X..X..X..X...X..X.
 .XXXX..X...X..X..X...X.XXX.XXX..
 .....X.XXXX...X..X..X..X...X..X.
 .XXXX..X.....XXX.XXX...XXX.X..X.
 ................................
 ....XXX...XXX....XX....X...X....
 ....X..X.X..X...X..X...XX..X....
 ....X...X...X..XXXXXX..X.X.X....
 ....X.......X..X....X..X..XX....
 ....X.......X..X....X..X...X....
 ................................
end
 drawscreen
 drawscreen
 drawscreen

 count=count-1
 if count>0 then goto titlecard

main
 if sc1=$00 && sc2=$00 && sc3>9 then count=10 : goto win  

 rem background is grey
 COLUBK=$86

 rem randomize whether ball is lethal and set playfield/ball color
 if random<128 then lethal=0 else lethal=1
 if lethal=0 then COLUPF=$0E else COLUPF=$40

 playfield:
 XXXXXX.........................X
 XXXXXX.........................X
 X..............................X
 X..............................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

 rem player0 is green
 COLUP0=$C0

 rem player1 is red
 COLUP1=$46

 player0:
 %00011100
 %00111000
 %00011000
 %00111000
 %00111000
 %00011100
 %01011100
 %11011100
 %11111000
 %11100100
 %11100010
 %11101010
 %01100100
 %00111100
 %00010000
end

 if joy0fire then player0:
 %00011100
 %00111000
 %00011000
 %00111000
 %00111000
 %00011100
 %01001100
 %11010111
 %11111000
 %11100100
 %11100010
 %11101010
 %01100100
 %00111100
 %00010000
end

 if pface=0 then player1:
 %00011100
 %00001100
 %00011000
 %00011011
 %00001111
 %00010111
 %00010111
 %00111110
 %01100111
 %00100111
 %00110010
 %00011100
end
 
 if pface=1 then player1:
 %00111000
 %00110000
 %00011000
 %11011000
 %11110000
 %11101000
 %11101000
 %01111100
 %11100110
 %11100100
 %01001100
 %00111000
end

 rem players' position
 player0x=p0posx
 player1x=p1posx

 drawscreen
 rem ==========/DRAW==========


 rem ==========LOGIC==========
 rem move sprites
 if joy0right && player0x<50 then p0posx=p0posx+1
 if joy0left && player0x>35 then p0posx=p0posx-1
 if joy1right && player1x<148 then pface=1 : p1posx=p1posx+3
 if joy1left && player1x>55 then pface=0 : p1posx=p1posx-3

 rem adjust power
 if joy0up && p0power<4 then p0power=p0power+0.1
 if joy0down && p0power>0 then p0power=p0power-0.1

 rem simulate gravity
 if !collision(playfield,player0) then player0y=player0y+1
 if !collision(playfield,player1) then player1y=player1y+1 

 rem position ball (held)
 if ballflies=0 then ballx=player0x+4 : i=player0y-7 : j=0 : bally=ballposy : gravity=0.0

 rem play ball, reset lethality
 if joy0fire && ballflies=0 then ballflies=1
 if collision(ball,playfield) then ballflies=0 : random=rand : goto main
 if collision(ball,player1) && lethal=1 && ballflies=1 then ballflies=0 : random=rand : goto hit
 if collision(ball,player1) && lethal=0 && ballflies=1 then ballflies=0 : random=rand : goto catch
 if ballflies=1 then goto ballfly else goto main

 rem position ball (flying)
ballfly
 ballx=ballx+1+p0power
 ballposy=ballposy+gravity
 bally=ballposy
 gravity=gravity+0.2
 goto main

 rem catch ball
catch
 score=score+1 
 goto main

 rem hit player1
hit
 COLUPF=$0E
 COLUBK=$40
 playfield:
 ................................
 ................................
 ................................
 .........................X......
 .........XXX.X.X.X.XXX.XXX......
 .........X.X.X.X.X.X.X.X.X......
 .........XXX.XXXXX.X.X.XXX......
 .........X......................
 ................................
 ................................
 ................................
end

 drawscreen
 drawscreen
 drawscreen

 COLUBK=$02
 playfield:
 XXXXXX.........................X
 XXXXXX.........................X
 X..............................X
 X..............................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

 drawscreen
 drawscreen
 drawscreen

 COLUBK=$40
 playfield:
 ................................
 ................................
 ................................
 .........................X......
 .........XXX.X.X.X.XXX.XXX......
 .........X.X.X.X.X.X.X.X.X......
 .........XXX.XXXXX.X.X.XXX......
 .........X......................
 ................................
 ................................
 ................................
end

 drawscreen
 drawscreen
 drawscreen

 player1:
 %0111110
 %0101010
 %1010101
end

 drawscreen
 drawscreen
 drawscreen

 COLUBK=$02
 playfield:
 XXXXXX.........................X
 XXXXXX.........................X
 X..............................X
 X..............................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

 drawscreen
 drawscreen
 drawscreen
 pfscore2 = pfscore2/4
 if switchreset then score=0 : pfscore2=21 : goto main
 if pfscore2>0 then goto main else goto hit


 rem player1 wins
win
 count=count-1
 if count>0 then goto win else count=20 : goto gtfo

gtfo
 COLUBK=$80
 playfield:
 ................................
 ................................
 ................................
 ...XXXX..XXXXX..XXXX..XXXX...X..
 ...X.......X....X.....X..X...X..
 ...X.XX....X....XXX...X..X...X..
 ...X..X....X....X.....X..X...X..
 ...XXXX....X....X.....XXXX......
 .............................X..
 ................................
 ................................
 ................................
end

 drawscreen
 count=count-1
 if count>0 then goto gtfo

 COLUBK=$02
 playfield:
 XXXXXX.........................X
 XXXXXX.........................X
 X..............................X
 X..............................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX.........................X
 XXXXXX..........................
 XXXXXX..........................
 XXXXXX..........................
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end
 count=70
 goto exit
 
exit
 player1:
 %00111000
 %00110000
 %00011000
 %11011000
 %11110000
 %11101000
 %11101000
 %01111000
 %01000100
 %01000010
 %11101010
 %11100100
 %01111100
 %00111000
end

 count=count-1
 player1x=player1x+1 
 drawscreen 
 if count>0 then goto exit

 COLUBK=$80
 playfield:
 .XXXX..XXXX..XXX.XXX...XXX.XXX..
 X......X...X..X..X..X..X...X..X.
 .XXXX..X...X..X..X...X.XXX.XXX..
 .....X.XXXX...X..X..X..X...X..X.
 .XXXX..X.....XXX.XXX...XXX.X..X.
 ................................
 .X..XXX...XXX....XX....X...X..X.
 .X..X..X.X..X...X..X...XX..X..X.
 .X..X...X...X..XXXXXX..X.X.X..X.
 ....X.......X..X....X..X..XX....
 .X..X.......X..X....X..X...X..X.
 ................................ 
end
 drawscreen
 if switchreset then score=0 : pfscore2=21 : goto main 
 rem ==========/LOGIC=========
