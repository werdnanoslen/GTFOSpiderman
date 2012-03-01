game
.L00 ;  rem ===========VARS===========

.L01 ;  dim p0posx = a

.L02 ;  dim p1posx = b

.
 ; 

.L03 ;  dim p0angle = c

.L04 ;  dim p1angle = d

.
 ; 

.L05 ;  dim ballflies = e

.L06 ;  dim pturn = f

.L07 ;  rem ===========/VARS==========

.
 ; 

.
 ; 

.L08 ;  rem ===========INIT===========

.L09 ;  set legacy 0.99

.
 ; 

.L010 ;  rem players' initial x pos

.L011 ;  p0posx = 32

	LDA #32
	STA p0posx
.L012 ;  p1posx = 150

	LDA #150
	STA p1posx
.
 ; 

.L013 ;  rem players' initial y pos

.L014 ;  player0y = 70

	LDA #70
	STA player0y
.L015 ;  player1y = 70

	LDA #70
	STA player1y
.
 ; 

.L016 ;  rem players' initial degree

.L017 ;  p0angle = 0

	LDA #0
	STA p0angle
.L018 ;  p1angle = 0

	LDA #0
	STA p1angle
.
 ; 

.L019 ;  rem player0 goes first

.L020 ;  ballflies = 0

	LDA #0
	STA ballflies
.L021 ;  pturn = 0

	LDA #0
	STA pturn
.L022 ;  rem ===========/INIT==========

.
 ; 

.
 ; 

.L023 ;  rem ===========DRAW===========

.main
 ; main

.
 ; 

.L024 ;  rem background is grey

.L025 ;  COLUBK = $02

	LDA #$02
	STA COLUBK
.
 ; 

.L026 ;  rem playfield is green

.L027 ;  COLUPF = $0E

	LDA #$0E
	STA COLUPF
.
 ; 

.L028 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %11000000, %10000000, %10000000, %11000000
	.byte %11100000, %11000000, %11000000, %11100000
	.byte %11111111, %11100111, %11100111, %11111111
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %11111111, %11111111, %11111111, %11111111
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.
 ; 

.L029 ;  rem player0 is red

.L030 ;  COLUP0 = $C0

	LDA #$C0
	STA COLUP0
.
 ; 

.L031 ;  rem player1 is blue

.L032 ;  COLUP1 = $FA

	LDA #$FA
	STA COLUP1
.
 ; 

.L033 ;  player0:

	LDA #<playerL033_0

	STA player0pointerlo
	LDA #>playerL033_0

	STA player0pointerhi
	LDA #11
	STA player0height
.
 ; 

.L034 ;  player1:

	LDA #<playerL034_1

	STA player1pointerlo
	LDA #>playerL034_1

	STA player1pointerhi
	LDA #11
	STA player1height
.
 ; 

.L035 ;  rem players' position

.L036 ;  player0x = p0posx

	LDA p0posx
	STA player0x
.L037 ;  player1x = p1posx

	LDA p1posx
	STA player1x
.
 ; 

.L038 ;  drawscreen

 jsr drawscreen
.L039 ;  rem ==========/DRAW==========

.
 ; 

.
 ; 

.L040 ;  rem ==========LOGIC==========

.L041 ;  rem move sprites

.L042 ;  if joy0right then p0posx = p0posx + 1

 lda #$80
 bit SWCHA
	BNE .skipL042
.condpart0
	INC p0posx
.skipL042
.L043 ;  if joy0left then p0posx = p0posx - 1

 lda #$40
 bit SWCHA
	BNE .skipL043
.condpart1
	DEC p0posx
.skipL043
.L044 ;  if joy1right then p1posx = p1posx + 1

 lda #8
 bit SWCHA
	BNE .skipL044
.condpart2
	INC p1posx
.skipL044
.L045 ;  if joy1left then p1posx = p1posx - 1

 lda #4
 bit SWCHA
	BNE .skipL045
.condpart3
	DEC p1posx
.skipL045
.
 ; 

.L046 ;  rem adjust angle

.L047 ;  if joy0up then p0angle = p0angle - 1

 lda #$10
 bit SWCHA
	BNE .skipL047
.condpart4
	DEC p0angle
.skipL047
.L048 ;  if joy0down then p0angle = p0angle + 1

 lda #$20
 bit SWCHA
	BNE .skipL048
.condpart5
	INC p0angle
.skipL048
.L049 ;  if joy1up then p1angle = p1angle - 1

 lda #1
 bit SWCHA
	BNE .skipL049
.condpart6
	DEC p1angle
.skipL049
.L050 ;  if joy1down then p1angle = p1angle + 1

 lda #2
 bit SWCHA
	BNE .skipL050
.condpart7
	INC p1angle
.skipL050
.
 ; 

.L051 ;  rem simulate gravity

.L052 ;  if !collision(playfield,player0) then player0y = player0y + 1

	BIT CXP0FB
	BMI .skipL052
.condpart8
	INC player0y
.skipL052
.L053 ;  if !collision(playfield,player1) then player1y = player1y + 1

	BIT CXP1FB
	BMI .skipL053
.condpart9
	INC player1y
.skipL053
.
 ; 

.L054 ;  rem position ball

.L055 ;  if ballflies = 0  &&  pturn = 0 then ballx = player0x + 4  :  bally = player0y - 4

	LDA ballflies
	CMP #0
     BNE .skipL055
.condpart10
	LDA pturn
	CMP #0
     BNE .skip10then
.condpart11
	LDA player0x
	CLC
	ADC #4
	STA ballx
	LDA player0y
	SEC
	SBC #4
	STA bally
.skip10then
.skipL055
.L056 ;  if ballflies = 0  &&  pturn = 1 then ballx = player1x + 4  :  bally = player1y - 4

	LDA ballflies
	CMP #0
     BNE .skipL056
.condpart12
	LDA pturn
	CMP #1
     BNE .skip12then
.condpart13
	LDA player1x
	CLC
	ADC #4
	STA ballx
	LDA player1y
	SEC
	SBC #4
	STA bally
.skip12then
.skipL056
.L057 ;  rem if pturn=0 && collision(ball,player1) then goto hit

.L058 ;  rem if pturn=1 && collision(ball,player0) then goto hit

.L059 ;  rem if ballflies then goto ballfly

.L060 ;  rem ==========/LOGIC=========

.L061 ;  goto main

 jmp .main

.
 ; 

.
 ; 

.L062 ;  rem ==========BALLFLY========

.L063 ;  rem ballfly

.L064 ;  rem if pturn=0 then ballx=player0x+velx*t : pturn=1

.L065 ;  rem if pturn=1 then ballx=player1x-velx*t : pturn=0

.L066 ;  rem bally=

.L067 ;  rem goto main

.L068 ;  rem ==========/BALLFLY=======

 if (<*) > (<(*+12))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL033_0

	.byte 0
	.byte  %01010100
	.byte  %10101010
	.byte  %11111110
	.byte  %01111000
	.byte  %00111110
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
 if (<*) > (<(*+12))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL034_1

	.byte 0
	.byte  %00101010
	.byte  %01010101
	.byte  %01111111
	.byte  %00011110
	.byte  %01111100
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 
 
 
