 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif
; This is a 2-line kernel!
kernel
 sta WSYNC
 lda #255
 sta TIM64T

 lda #1
 sta VDELBL
 sta VDELP0
 ldx ballheight
 inx
 inx
 stx temp4
 lda player1y
 sta temp3

 ifconst shakescreen
   jsr doshakescreen
 else
   ldx missile0height
   inx
 endif

 inx
 stx stack1

 lda bally
 sta stack2

 lda player0y
 ldx #0
 sta WSYNC
 stx GRP0
 stx GRP1
 stx PF1
 stx PF2
 stx CXCLR
 ifconst readpaddle
   stx paddle
 else
   sleep 3
 endif

 sta temp2,x

 ;store these so they can be retrieved later
 ifnconst pfres
   ldx #128-44
 else
   ldx #132-pfres*4
 endif

 inc player1y

 lda missile0y
 sta temp5
 lda missile1y
 sta temp6

 lda playfieldpos
 sta temp1
 
 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif

 clc
 sbc playfieldpos
 sta playfieldpos
 jmp .startkernel

.skipDrawP0
 lda #0
 tay
 jmp .continueP0

.skipDrawP1
 lda #0
 tay
 jmp .continueP1

.kerloop ; enter at cycle 59??

continuekernel
 sleep 2
continuekernel2
 lda ballheight
 
 ifconst pfres
 ldy playfield+pfres*4-132,x
 sty PF1 ;3
 ldy playfield+pfres*4-131,x
 sty PF2 ;3
 ldy playfield+pfres*4-129,x
 sty PF1 ; 3 too early?
 ldy playfield+pfres*4-130,x
 sty PF2 ;3
 else
 ldy playfield+44-128,x ;4
 sty PF1 ;3
 ldy playfield+45-128,x ;4
 sty PF2 ;3
 ldy playfield+47-128,x ;4
 sty PF1 ; 3 too early?
 ldy playfield+46-128,x;4
 sty PF2 ;3
 endif

 dcp bally
 rol
 rol
; rol
; rol
goback
 sta ENABL 
.startkernel
 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawP1 ;2
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continueP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
   rol;2
   rol;2
   sta ENAM1 ;3
 else
   lda (player1color),y
   sta COLUP1
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif

 ifconst pfres
 lda playfield+pfres*4-132,x 
 sta PF1 ;3
 lda playfield+pfres*4-131,x 
 sta PF2 ;3
 lda playfield+pfres*4-129,x 
 sta PF1 ; 3 too early?
 lda playfield+pfres*4-130,x 
 sta PF2 ;3
 else
 lda playfield+44-128,x ;4
 sta PF1 ;3
 lda playfield+45-128,x ;4
 sta PF2 ;3
 lda playfield+47-128,x ;4
 sta PF1 ; 3 too early?
 lda playfield+46-128,x;4
 sta PF2 ;3
 endif 
; sleep 3

 lda player0height
 dcp player0y
 bcc .skipDrawP0
 ldy player0y
 lda (player0pointer),y
.continueP0
 sta GRP0

 ifnconst no_blank_lines
 ifnconst playercolors
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
 else
   lda (player0color),y
   sta player0colorstore
   sleep 6
 endif
   dec temp1
   bne continuekernel
 else
   dec temp1
   beq altkernel2
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle
   inc paddle
   jmp continuekernel2
noreadpaddle
   sleep 2
   jmp continuekernel
 else
 ifnconst playercolors 
 ifconst PFcolors
   txa
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
   jmp continuekernel
 else
   sleep 12
 endif
 else
   lda (player0color),y
   sta player0colorstore
   sleep 4
 endif
   jmp continuekernel
 endif
altkernel2
   txa
   sbx #252
   bmi lastkernelline
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
   jmp continuekernel
 endif

altkernel

 ifconst PFmaskvalue
   lda #PFmaskvalue
 else
   lda #0
 endif
 sta PF1
 sta PF2


 ;sleep 3

 ;28 cycles to fix things
 ;minus 11=17

; lax temp4
; clc
 txa
 sbx #252

 bmi lastkernelline

 ifconst PFcolorandheight
   ldy playfieldcolorandheight-87,x
 ifnconst backgroundchange
   sty COLUPF
 else
   sty COLUBK
 endif
   lda playfieldcolorandheight-88,x
   sta.w temp1
 endif
 ifconst PFheights
   lsr
   lsr
   tay
   lda (pfheighttable),y
   sta.w temp1
 endif
 ifconst PFcolors
   tay
   lda (pfcolortable),y
 ifnconst backgroundchange
   sta COLUPF
 else
   sta COLUBK
 endif
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 ifnconst PFcolorandheight
 ifnconst PFcolors
 ifnconst PFheights
 ifnconst no_blank_lines
 ; read paddle 0
 ; lo-res paddle read
  ; bit INPT0
  ; bmi paddleskipread
  ; inc paddle0
;donepaddleskip
   sleep 10
 ifconst pfrowheight
   lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif
   sta temp1
 endif
 endif
 endif
 endif
 

 lda ballheight
 dcp bally
 sbc temp4


 jmp goback


 ifnconst no_blank_lines
lastkernelline
 ifnconst PFcolors
   sleep 10
 else
   ldy #124
   lda (pfcolortable),y
   sta COLUPF
 endif

 ifconst PFheights
 ldx #1
 sleep 4
 else
 ldx playfieldpos
 sleep 3
 endif

 jmp enterlastkernel

 else
lastkernelline
 
 ifconst PFheights
 ldx #1
 sleep 5
 else
   ldx playfieldpos
 sleep 4
 endif

   cpx #1
   bne .enterfromNBL
   jmp no_blank_lines_bailout
 endif

 if ((<*)>$d5)
 align 256
 endif
 ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
 sleep 2
 lda #0
 jmp .continuelastP1

.endkerloop ; enter at cycle 59??
 
 nop

.enterfromNBL
 ifconst pfres
 ldy.w playfield+pfres*4-4
 sty PF1 ;3
 ldy.w playfield+pfres*4-3
 sty PF2 ;3
 ldy.w playfield+pfres*4-1
 sty PF1 ; possibly too early?
 ldy.w playfield+pfres*4-2
 sty PF2 ;3
 else
 ldy.w playfield+44
 sty PF1 ;3
 ldy.w playfield+45
 sty PF2 ;3
 ldy.w playfield+47
 sty PF1 ; possibly too early?
 ldy.w playfield+46
 sty PF2 ;3
 endif

enterlastkernel
 lda ballheight

; tya
 dcp bally
; sleep 4

; sbc stack3
 rol
 rol
 sta ENABL 

 lda player1height ;3
 dcp player1y ;5
 bcc .skipDrawlastP1
 ldy player1y ;3
 lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
			; so it doesn't cross a page boundary!

.continuelastP1
 sta GRP1 ;3

 ifnconst player1colors
   lda missile1height ;3
   dcp missile1y ;5
 else
   lda (player1color),y
   sta COLUP1
 endif

 dex
 ;dec temp4 ; might try putting this above PF writes
 beq endkernel


 ifconst pfres
 ldy.w playfield+pfres*4-4
 sty PF1 ;3
 ldy.w playfield+pfres*4-3
 sty PF2 ;3
 ldy.w playfield+pfres*4-1
 sty PF1 ; possibly too early?
 ldy.w playfield+pfres*4-2
 sty PF2 ;3
 else
 ldy.w playfield+44
 sty PF1 ;3
 ldy.w playfield+45
 sty PF2 ;3
 ldy.w playfield+47
 sty PF1 ; possibly too early?
 ldy.w playfield+46
 sty PF2 ;3
 endif

 ifnconst player1colors
   rol;2
   rol;2
   sta ENAM1 ;3
 else
 ifnconst playercolors
   sleep 7
 else
   lda.w player0colorstore
   sta COLUP0
 endif
 endif
 
 lda.w player0height
 dcp player0y
 bcc .skipDrawlastP0
 ldy player0y
 lda (player0pointer),y
.continuelastP0
 sta GRP0



 ifnconst no_blank_lines
   lda missile0height ;3
   dcp missile0y ;5
   sbc stack1
   sta ENAM0 ;3
   jmp .endkerloop
 else
 ifconst readpaddle
   ldy currentpaddle
   lda INPT0,y
   bpl noreadpaddle2
   inc paddle
   jmp .endkerloop
noreadpaddle2
   sleep 4
   jmp .endkerloop
 else ; no_blank_lines and no paddle reading
 sleep 14
 jmp .endkerloop
 endif
 endif


;  ifconst donepaddleskip
;paddleskipread
 ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
 ; plus we get a lo-res paddle read
; bmi donepaddleskip
;  endif

.skipDrawlastP0
 sleep 2
 lda #0
 jmp .continuelastP0

 ifconst no_blank_lines
no_blank_lines_bailout
 ldx #0
 endif

endkernel
 ; 6 digit score routine
 stx PF1
 stx PF2
 stx PF0
 clc

 ifconst pfrowheight
 lda #pfrowheight+2
 else
 ifnconst pfres
   lda #10
 else
   lda #(96/pfres)+2 ; try to come close to the real size
 endif
 endif

 sbc playfieldpos
 sta playfieldpos
 txa

 ifconst shakescreen
   bit shakescreen
   bmi noshakescreen2
   ldx #$3D
noshakescreen2
 endif

   sta WSYNC,x

;                STA WSYNC ;first one, need one more
 sta REFP0
 sta REFP1
                STA GRP0
                STA GRP1
 ;               STA PF1
   ;             STA PF2
 sta HMCLR
 sta ENAM0
 sta ENAM1
 sta ENABL

 lda temp2 ;restore variables that were obliterated by kernel
 sta player0y
 lda temp3
 sta player1y
 ifnconst player1colors
   lda temp6
   sta missile1y
 endif
 ifnconst playercolors
 ifnconst readpaddle
   lda temp5
   sta missile0y
 endif
 endif
 lda stack2
 sta bally

 ifconst no_blank_lines
 sta WSYNC
 endif

 lda INTIM
 clc
 ifnconst vblank_time
 adc #43+12+87
 else
 adc #vblank_time+12+87
 endif
; sta WSYNC
 sta TIM64T

 ifconst minikernel
 jsr minikernel
 endif

 ; now reassign temp vars for score pointers

; score pointers contain:
; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
; swap lo2->temp1
; swap lo4->temp3
; swap lo6->temp5
 ifnconst noscore
 lda scorepointers+1
; ldy temp1
 sta temp1
; sty scorepointers+1

 lda scorepointers+3
; ldy temp3
 sta temp3
; sty scorepointers+3


 sta HMCLR
 tsx
 stx stack1 
 ldx #$10
 stx HMP0

 sta WSYNC
 ldx #0
                STx GRP0
                STx GRP1 ; seems to be needed because of vdel

 lda scorepointers+5
; ldy temp5
 sta temp5,x
; sty scorepointers+5
 lda #>scoretable
 sta scorepointers+1
 sta scorepointers+3
 sta scorepointers+5,x
 sta temp2,x
 sta temp4,x
 sta temp6,x
                LDY #7
                STA RESP0
                STA RESP1


        LDA #$03
        STA NUSIZ0
        STA NUSIZ1,x
        STA VDELP0
        STA VDELP1
        LDA #$20
        STA HMP1
               LDA scorecolor 
;               STA HMCLR
;               STA WSYNC; second one
                STA HMOVE ; cycle 73 ?

                STA COLUP0
                STA COLUP1
 lda  (scorepointers),y
 sta  GRP0
 ifconst pfscore
 lda pfscorecolor
 sta COLUPF
 endif
 lda  (scorepointers+8),y
 sta WSYNC
 sleep 2
 jmp beginscore

 if ((<*)>$d4)
 align 256 ; kludge that potentially wastes space!  should be fixed!
 endif

loop2
 lda  (scorepointers),y     ;+5  68  204
 sta  GRP0            ;+3  71  213      D1     --      --     --
 ifconst pfscore
 lda.w pfscore1
 sta PF1
 else
 sleep 7
 endif
 ; cycle 0
 lda  (scorepointers+$8),y  ;+5   5   15
beginscore
 sta  GRP1            ;+3   8   24      D1     D1      D2     --
 lda  (scorepointers+$6),y  ;+5  13   39
 sta  GRP0            ;+3  16   48      D3     D1      D2     D2
 lax  (scorepointers+$2),y  ;+5  29   87
 txs
 lax  (scorepointers+$4),y  ;+5  36  108
 sleep 3

 ifconst pfscore
 lda pfscore2
 sta PF1
 else
 sleep 6
 endif

 lda  (scorepointers+$A),y  ;+5  21   63
 stx  GRP1            ;+3  44  132      D3     D3      D4     D2!
 tsx
 stx  GRP0            ;+3  47  141      D5     D3!     D4     D4
 sta  GRP1            ;+3  50  150      D5     D5      D6     D4!
 sty  GRP0            ;+3  53  159      D4*    D5!     D6     D6
 dey
 bpl  loop2           ;+2  60  180

 ldx stack1 
 txs
; lda scorepointers+1
 ldy temp1
; sta temp1
 sty scorepointers+1

                LDA #0   
 sta PF1
               STA GRP0
                STA GRP1
        STA VDELP0
        STA VDELP1;do we need these
        STA NUSIZ0
        STA NUSIZ1

; lda scorepointers+3
 ldy temp3
; sta temp3
 sty scorepointers+3

; lda scorepointers+5
 ldy temp5
; sta temp5
 sty scorepointers+5
 endif ;noscore
 LDA #%11000010
 sta WSYNC
 STA VBLANK
 RETURN

 ifconst shakescreen
doshakescreen
   bit shakescreen
   bmi noshakescreen
   sta WSYNC
noshakescreen
   ldx missile0height
   inx
   rts
 endif

start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifconst pfrowheight
 lda pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*4-1
 else
 ldx #47
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 asl ; multiply y pos by 4
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 asl ; multiply by 4
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 iny
 iny
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
drawscreen
 ifconst debugscore
   ldx #14
   lda INTIM ; display # cycles left in the score

 ifconst mincycles
 lda mincycles 
 cmp INTIM
 lda mincycles
 bcc nochange
 lda INTIM
 sta mincycles
nochange
 endif

;   cmp #$2B
;   bcs no_cycles_left
   bmi cycles_left
   ldx #64
   eor #$ff ;make negative
cycles_left
   stx scorecolor
   and #$7f ; clear sign bit
   tax
   lda scorebcd,x
   sta score+2
   lda scorebcd1,x
   sta score+1
   jmp done_debugscore   
scorebcd
 .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
 .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
 .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
 .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
 .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
 .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
 .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
 .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
done_debugscore
 endif

 ifconst debugcycles
   lda INTIM ; if we go over, it mucks up the background color
;   cmp #$2B
;   BCC overscan
   bmi overscan
   sta COLUBK
   bcs doneoverscan
 endif

 
overscan
 lda INTIM ;wait for sync
 bmi overscan
doneoverscan
;do VSYNC
 lda #2
 sta WSYNC
 sta VSYNC
 STA WSYNC
 STA WSYNC
 LDA #0
 STA WSYNC
 STA VSYNC
 sta VBLANK
 ifnconst overscan_time
 lda #37+128
 else
 lda #overscan_time+128
 endif
 sta TIM64T

 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop
 lda player0x,x
 sec
 sbc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop
 endif
 endif
 if (<*)>$F0
 align 256, $EA
 endif
  sta WSYNC
  ldx #4
  SLEEP 3
HorPosLoop       ;     5
  lda player0x,X  ;+4   9
  sec           ;+2  11
DivideLoop
  sbc #15
  bcs DivideLoop;+4  15
  sta temp1,X    ;+4  19
  sta RESP0,X   ;+4  23
  sta WSYNC
  dex
  bpl HorPosLoop;+5   5
                ;     4

  ldx #4
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 18

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 32

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 46

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 60

  dex
  ldy temp1,X
  lda repostable-256,Y
  sta HMP0,X    ;+14 74

  sta WSYNC
 
  sta HMOVE     ;+3   3


 ifconst legacy
 if legacy < 100
 ldx #4
adjustloop2
 lda player0x,x
 clc
 adc #14 ;?
 sta player0x,x
 dex
 bpl adjustloop2
 endif
 endif




;set score pointers
 lax score+2
 jsr scorepointerset
 sty scorepointers+5
 stx scorepointers+2
 lax score+1
 jsr scorepointerset
 sty scorepointers+4
 stx scorepointers+1
 lax score
 jsr scorepointerset
 sty scorepointers+3
 stx scorepointers

vblk
; run possible vblank bB code
 ifconst vblank_bB_code
   jsr vblank_bB_code
 endif
vblk2
 LDA INTIM
 bmi vblk2
 jmp kernel
 

    .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
    .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
 and #$0F
 asl
 asl
 asl
 adc #<scoretable
 tay 
 txa
; and #$F0
; lsr
 asr #$F0
 adc #<scoretable
 tax
 rts
game
.L00 ;  rem ===========SETUP==========

.L01 ;  set legacy 0.99

.L02 ;  set optimization inlinerand

.L03 ;  const pfscore = 1

.L04 ;  rem ===========/SETUP=========

.
 ; 

.
 ; 

.L05 ;  rem ===========VARS===========

.L06 ;  dim sc1  =  score

.L07 ;  dim sc2  =  score + 1

.L08 ;  dim sc3  =  score + 2

.
 ; 

.L09 ;  dim p0posx = a

.L010 ;  dim p1posx = b

.
 ; 

.L011 ;  dim p0power = c.d

.
 ; 

.L012 ;  dim ballflies = e

.L013 ;  dim pface = f

.
 ; 

.L014 ;  dim gravity = g.h

.
 ; 

.L015 ;  dim ballposy = i.j

.
 ; 

.L016 ;  dim random = k

.L017 ;  dim lethal = l

.L018 ;  dim count = m

.L019 ;  rem ===========/VARS==========

.
 ; 

.
 ; 

.L020 ;  rem ===========INIT===========

.L021 ;  rem players' initial x pos

.L022 ;  p0posx = 40

	LDA #40
	STA p0posx
.L023 ;  p1posx = 100

	LDA #100
	STA p1posx
.
 ; 

.L024 ;  rem players' initial y pos

.L025 ;  player0y = 31

	LDA #31
	STA player0y
.L026 ;  player1y = 70

	LDA #70
	STA player1y
.
 ; 

.L027 ;  rem players' initial power/score

.L028 ;  p0power = 0.0

	LDX #0
	STX d
	LDA #0
	STA p0power
.L029 ;  pfscore2 = 21

	LDA #21
	STA pfscore2
.L030 ;  score = 0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L031 ;  rem ball initial placement

.L032 ;  ballflies = 0

	LDA #0
	STA ballflies
.L033 ;  pface = 0

	LDA #0
	STA pface
.L034 ;  gravity = 0.0

	LDX #0
	STX h
	LDA #0
	STA gravity
.L035 ;  ballposy = 0.0

	LDX #0
	STX j
	LDA #0
	STA ballposy
.L036 ;  lethal = 0

	LDA #0
	STA lethal
.L037 ;  count = 10

	LDA #10
	STA count
.L038 ;  rem ===========/INIT==========

.
 ; 

.
 ; 

.L039 ;  rem ===========DRAW===========

.titlecard
 ; titlecard

.L040 ;  COLUBK = $40

	LDA #$40
	STA COLUBK
.L041 ;  COLUPF = $70

	LDA #$70
	STA COLUPF
.
 ; 

.L042 ;  drawscreen

 jsr drawscreen
.L043 ;  drawscreen

 jsr drawscreen
.L044 ;  drawscreen

 jsr drawscreen
.
 ; 

.L045 ;  COLUBK = $70

	LDA #$70
	STA COLUBK
.L046 ;  COLUPF = $40

	LDA #$40
	STA COLUPF
.L047 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel0
PF_data0
	.byte %01111001, %11100111, %01110001, %00111011
	.byte %10000001, %01001000, %01001001, %01001000
	.byte %01111001, %01001000, %01000101, %00111011
	.byte %00000101, %01000111, %01001001, %01001000
	.byte %01111001, %11100000, %01110001, %01001011
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00001110, %00011100, %01100001, %00001000
	.byte %00001001, %00010010, %10010001, %00001001
	.byte %00001000, %10010001, %11111001, %00001010
	.byte %00001000, %10010000, %00001001, %00001100
	.byte %00001000, %10010000, %00001001, %00001000
	.byte %00000000, %00000000, %00000000, %00000000
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.L048 ;  drawscreen

 jsr drawscreen
.L049 ;  drawscreen

 jsr drawscreen
.L050 ;  drawscreen

 jsr drawscreen
.
 ; 

.L051 ;  count = count - 1

	DEC count
.L052 ;  if count > 0 then goto titlecard

	LDA #0
	CMP count
     BCS .skipL052
.condpart0
 jmp .titlecard

.skipL052
.
 ; 

.main
 ; main

.L053 ;  if sc1 = $00  &&  sc2 = $00  &&  sc3 > 9 then count = 10  :  goto win

	LDA sc1
	CMP #$00
     BNE .skipL053
.condpart1
	LDA sc2
	CMP #$00
     BNE .skip1then
.condpart2
	LDA #9
	CMP sc3
     BCS .skip2then
.condpart3
	LDA #10
	STA count
 jmp .win

.skip2then
.skip1then
.skipL053
.
 ; 

.L054 ;  rem background is grey

.L055 ;  COLUBK = $86

	LDA #$86
	STA COLUBK
.
 ; 

.L056 ;  rem randomize whether ball is lethal and set playfield/ball color

.L057 ;  if random < 128 then lethal = 0 else lethal = 1

	LDA random
	CMP #128
     BCS .skipL057
.condpart4
	LDA #0
	STA lethal
 jmp .skipelse0
.skipL057
	LDA #1
	STA lethal
.skipelse0
.L058 ;  if lethal = 0 then COLUPF = $0E else COLUPF = $40

	LDA lethal
	CMP #0
     BNE .skipL058
.condpart5
	LDA #$0E
	STA COLUPF
 jmp .skipelse1
.skipL058
	LDA #$40
	STA COLUPF
.skipelse1
.
 ; 

.L059 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel1
PF_data1
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %11111111, %11111111, %11111111, %11111111
pflabel1
	lda PF_data1,x
	sta playfield,x
	dex
	bpl pflabel1
.
 ; 

.L060 ;  rem player0 is green

.L061 ;  COLUP0 = $C0

	LDA #$C0
	STA COLUP0
.
 ; 

.L062 ;  rem player1 is red

.L063 ;  COLUP1 = $46

	LDA #$46
	STA COLUP1
.
 ; 

.L064 ;  player0:

	LDA #<playerL064_0

	STA player0pointerlo
	LDA #>playerL064_0

	STA player0pointerhi
	LDA #15
	STA player0height
.
 ; 

.L065 ;  if joy0fire then player0:

 lda #$80
 bit INPT4
	BNE .skipL065
.condpart6
	LDA #<player6then_0

	STA player0pointerlo
	LDA #>player6then_0

	STA player0pointerhi
	LDA #15
	STA player0height
.skipL065
.
 ; 

.L066 ;  if pface = 0 then player1:

	LDA pface
	CMP #0
     BNE .skipL066
.condpart7
	LDA #<player7then_1

	STA player1pointerlo
	LDA #>player7then_1

	STA player1pointerhi
	LDA #12
	STA player1height
.skipL066
.
 ; 

.L067 ;  if pface = 1 then player1:

	LDA pface
	CMP #1
     BNE .skipL067
.condpart8
	LDA #<player8then_1

	STA player1pointerlo
	LDA #>player8then_1

	STA player1pointerhi
	LDA #12
	STA player1height
.skipL067
.
 ; 

.L068 ;  rem players' position

.L069 ;  player0x = p0posx

	LDA p0posx
	STA player0x
.L070 ;  player1x = p1posx

	LDA p1posx
	STA player1x
.
 ; 

.L071 ;  drawscreen

 jsr drawscreen
.L072 ;  rem ==========/DRAW==========

.
 ; 

.
 ; 

.L073 ;  rem ==========LOGIC==========

.L074 ;  rem move sprites

.L075 ;  if joy0right  &&  player0x < 50 then p0posx = p0posx + 1

 lda #$80
 bit SWCHA
	BNE .skipL075
.condpart9
	LDA player0x
	CMP #50
     BCS .skip9then
.condpart10
	INC p0posx
.skip9then
.skipL075
.L076 ;  if joy0left  &&  player0x > 35 then p0posx = p0posx - 1

 lda #$40
 bit SWCHA
	BNE .skipL076
.condpart11
	LDA #35
	CMP player0x
     BCS .skip11then
.condpart12
	DEC p0posx
.skip11then
.skipL076
.L077 ;  if joy1right  &&  player1x < 148 then pface = 1  :  p1posx = p1posx + 3

 lda #8
 bit SWCHA
	BNE .skipL077
.condpart13
	LDA player1x
	CMP #148
     BCS .skip13then
.condpart14
	LDA #1
	STA pface
	LDA p1posx
	CLC
	ADC #3
	STA p1posx
.skip13then
.skipL077
.L078 ;  if joy1left  &&  player1x > 55 then pface = 0  :  p1posx = p1posx - 3

 lda #4
 bit SWCHA
	BNE .skipL078
.condpart15
	LDA #55
	CMP player1x
     BCS .skip15then
.condpart16
	LDA #0
	STA pface
	LDA p1posx
	SEC
	SBC #3
	STA p1posx
.skip15then
.skipL078
.
 ; 

.L079 ;  rem adjust power

.L080 ;  if joy0up  &&  p0power < 4 then p0power = p0power + 0.1

 lda #$10
 bit SWCHA
	BNE .skipL080
.condpart17
	LDA p0power
	CMP #4
     BCS .skip17then
.condpart18
	LDA d
	CLC 
	ADC #25
	STA d
	LDA p0power
	ADC #0
	STA p0power
.skip17then
.skipL080
.L081 ;  if joy0down  &&  p0power > 0 then p0power = p0power - 0.1

 lda #$20
 bit SWCHA
	BNE .skipL081
.condpart19
	LDA #0
	CMP p0power
     BCS .skip19then
.condpart20
	LDA d
	SEC 
	SBC #25
	STA d
	LDA p0power
	SBC #0
	STA p0power
.skip19then
.skipL081
.
 ; 

.L082 ;  rem simulate gravity

.L083 ;  if !collision(playfield,player0) then player0y = player0y + 1

	BIT CXP0FB
	BMI .skipL083
.condpart21
	INC player0y
.skipL083
.L084 ;  if !collision(playfield,player1) then player1y = player1y + 1

	BIT CXP1FB
	BMI .skipL084
.condpart22
	INC player1y
.skipL084
.
 ; 

.L085 ;  rem position ball (held)

.L086 ;  if ballflies = 0 then ballx = player0x + 4  :  i = player0y - 7  :  j = 0  :  bally = ballposy  :  gravity = 0.0

	LDA ballflies
	CMP #0
     BNE .skipL086
.condpart23
	LDA player0x
	CLC
	ADC #4
	STA ballx
	LDA player0y
	SEC
	SBC #7
	STA i
	LDA #0
	STA j
	LDA ballposy
	STA bally
	LDX #0
	STX h
	LDA #0
	STA gravity
.skipL086
.
 ; 

.L087 ;  rem play ball, reset lethality

.L088 ;  if joy0fire  &&  ballflies = 0 then ballflies = 1

 lda #$80
 bit INPT4
	BNE .skipL088
.condpart24
	LDA ballflies
	CMP #0
     BNE .skip24then
.condpart25
	LDA #1
	STA ballflies
.skip24then
.skipL088
.L089 ;  if collision(ball,playfield) then ballflies = 0  :  random = rand  :  goto main

	BIT CXBLPF
	BPL .skipL089
.condpart26
	LDA #0
	STA ballflies
        lda rand
        lsr
 ifconst rand16
        rol rand16
 endif
        bcc *+4
        eor #$B4
        sta rand
 ifconst rand16
        eor rand16
 endif
	STA random
 jmp .main

.skipL089
.L090 ;  if collision(ball,player1)  &&  lethal = 1  &&  ballflies = 1 then ballflies = 0  :  random = rand  :  goto hit

	BIT CXP1FB
	BVC .skipL090
.condpart27
	LDA lethal
	CMP #1
     BNE .skip27then
.condpart28
	LDA ballflies
	CMP #1
     BNE .skip28then
.condpart29
	LDA #0
	STA ballflies
        lda rand
        lsr
 ifconst rand16
        rol rand16
 endif
        bcc *+4
        eor #$B4
        sta rand
 ifconst rand16
        eor rand16
 endif
	STA random
 jmp .hit

.skip28then
.skip27then
.skipL090
.L091 ;  if collision(ball,player1)  &&  lethal = 0  &&  ballflies = 1 then ballflies = 0  :  random = rand  :  goto catch

	BIT CXP1FB
	BVC .skipL091
.condpart30
	LDA lethal
	CMP #0
     BNE .skip30then
.condpart31
	LDA ballflies
	CMP #1
     BNE .skip31then
.condpart32
	LDA #0
	STA ballflies
        lda rand
        lsr
 ifconst rand16
        rol rand16
 endif
        bcc *+4
        eor #$B4
        sta rand
 ifconst rand16
        eor rand16
 endif
	STA random
 jmp .catch

.skip31then
.skip30then
.skipL091
.L092 ;  if ballflies = 1 then goto ballfly else goto main

	LDA ballflies
	CMP #1
     BNE .skipL092
.condpart33
 jmp .ballfly
 jmp .skipelse2
.skipL092
 jmp .main

.skipelse2
.
 ; 

.L093 ;  rem position ball (flying)

.ballfly
 ; ballfly

.L094 ;  ballx = ballx + 1 + p0power

; complex statement detected
	LDA ballx
	CLC
	ADC #1
	CLC
	ADC p0power
	STA ballx
.L095 ;  ballposy = ballposy + gravity

	LDA j
	CLC 
	ADC h
	STA j
	LDA ballposy
	ADC gravity
	STA ballposy
.L096 ;  bally = ballposy

	LDA ballposy
	STA bally
.L097 ;  gravity = gravity + 0.2

	LDA h
	CLC 
	ADC #51
	STA h
	LDA gravity
	ADC #0
	STA gravity
.L098 ;  goto main

 jmp .main

.
 ; 

.L099 ;  rem catch ball

.catch
 ; catch

.L0100 ;  score = score + 1

	SED
	CLC
	LDA score+2
	ADC #$01
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L0101 ;  goto main

 jmp .main

.
 ; 

.L0102 ;  rem hit player1

.hit
 ; hit

.L0103 ;  COLUPF = $0E

	LDA #$0E
	STA COLUPF
.L0104 ;  COLUBK = $40

	LDA #$40
	STA COLUBK
.L0105 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel2
PF_data2
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000010
	.byte %00000000, %10101110, %01011101, %00000011
	.byte %00000000, %10101010, %01010101, %00000010
	.byte %00000000, %11101110, %11010101, %00000011
	.byte %00000000, %00000010, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
pflabel2
	lda PF_data2,x
	sta playfield,x
	dex
	bpl pflabel2
.
 ; 

.L0106 ;  drawscreen

 jsr drawscreen
.L0107 ;  drawscreen

 jsr drawscreen
.L0108 ;  drawscreen

 jsr drawscreen
.
 ; 

.L0109 ;  COLUBK = $02

	LDA #$02
	STA COLUBK
.L0110 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel3
PF_data3
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %11111111, %11111111, %11111111, %11111111
pflabel3
	lda PF_data3,x
	sta playfield,x
	dex
	bpl pflabel3
.
 ; 

.L0111 ;  drawscreen

 jsr drawscreen
.L0112 ;  drawscreen

 jsr drawscreen
.L0113 ;  drawscreen

 jsr drawscreen
.
 ; 

.L0114 ;  COLUBK = $40

	LDA #$40
	STA COLUBK
.L0115 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel4
PF_data4
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000010
	.byte %00000000, %10101110, %01011101, %00000011
	.byte %00000000, %10101010, %01010101, %00000010
	.byte %00000000, %11101110, %11010101, %00000011
	.byte %00000000, %00000010, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
pflabel4
	lda PF_data4,x
	sta playfield,x
	dex
	bpl pflabel4
.
 ; 

.L0116 ;  drawscreen

 jsr drawscreen
.L0117 ;  drawscreen

 jsr drawscreen
.L0118 ;  drawscreen

 jsr drawscreen
.
 ; 

.L0119 ;  player1:

	LDA #<playerL0119_1

	STA player1pointerlo
	LDA #>playerL0119_1

	STA player1pointerhi
	LDA #3
	STA player1height
.
 ; 

.L0120 ;  drawscreen

 jsr drawscreen
.L0121 ;  drawscreen

 jsr drawscreen
.L0122 ;  drawscreen

 jsr drawscreen
.
 ; 

.L0123 ;  COLUBK = $02

	LDA #$02
	STA COLUBK
.L0124 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel5
PF_data5
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %11111111, %11111111, %11111111, %11111111
pflabel5
	lda PF_data5,x
	sta playfield,x
	dex
	bpl pflabel5
.
 ; 

.L0125 ;  drawscreen

 jsr drawscreen
.L0126 ;  drawscreen

 jsr drawscreen
.L0127 ;  drawscreen

 jsr drawscreen
.L0128 ;  pfscore2  =  pfscore2 / 4

	LDA pfscore2
	lsr
	lsr
	STA pfscore2
.L0129 ;  if switchreset then score = 0  :  pfscore2 = 21  :  goto main

 lda #1
 bit SWCHB
	BNE .skipL0129
.condpart34
	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
	LDA #21
	STA pfscore2
 jmp .main

.skipL0129
.L0130 ;  if pfscore2 > 0 then goto main else goto hit

	LDA #0
	CMP pfscore2
     BCS .skipL0130
.condpart35
 jmp .main
 jmp .skipelse3
.skipL0130
 jmp .hit

.skipelse3
.
 ; 

.
 ; 

.L0131 ;  rem player1 wins

.win
 ; win

.L0132 ;  count = count - 1

	DEC count
.L0133 ;  if count > 0 then goto win else count = 20  :  goto gtfo

	LDA #0
	CMP count
     BCS .skipL0133
.condpart36
 jmp .win
 jmp .skipelse4
.skipL0133
	LDA #20
	STA count
 jmp .gtfo

.skipelse4
.
 ; 

.gtfo
 ; gtfo

.L0134 ;  COLUBK = $80

	LDA #$80
	STA COLUBK
.L0135 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel6
PF_data6
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00011110, %00111110, %11110011, %00100011
	.byte %00010000, %00001000, %10000010, %00100010
	.byte %00010110, %00001000, %11100010, %00100010
	.byte %00010010, %00001000, %10000010, %00100010
	.byte %00011110, %00001000, %10000011, %00000011
	.byte %00000000, %00000000, %00000000, %00100000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000
pflabel6
	lda PF_data6,x
	sta playfield,x
	dex
	bpl pflabel6
.
 ; 

.L0136 ;  drawscreen

 jsr drawscreen
.L0137 ;  count = count - 1

	DEC count
.L0138 ;  if count > 0 then goto gtfo

	LDA #0
	CMP count
     BCS .skipL0138
.condpart37
 jmp .gtfo

.skipL0138
.
 ; 

.L0139 ;  COLUBK = $02

	LDA #$02
	STA COLUBK
.L0140 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel7
PF_data7
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %10000000, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %10000000
	.byte %11111100, %00000000, %00000000, %00000000
	.byte %11111100, %00000000, %00000000, %00000000
	.byte %11111100, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %11111111, %11111111
	.byte %11111111, %11111111, %11111111, %11111111
pflabel7
	lda PF_data7,x
	sta playfield,x
	dex
	bpl pflabel7
.L0141 ;  count = 70

	LDA #70
	STA count
.L0142 ;  goto exit

 jmp .exit

.
 ; 

.exit
 ; exit

.L0143 ;  player1:

	LDA #<playerL0143_1

	STA player1pointerlo
	LDA #>playerL0143_1

	STA player1pointerhi
	LDA #14
	STA player1height
.
 ; 

.L0144 ;  count = count - 1

	DEC count
.L0145 ;  player1x = player1x + 1

	INC player1x
.L0146 ;  drawscreen

 jsr drawscreen
.L0147 ;  if count > 0 then goto exit

	LDA #0
	CMP count
     BCS .skipL0147
.condpart38
 jmp .exit

.skipL0147
.
 ; 

.L0148 ;  COLUBK = $80

	LDA #$80
	STA COLUBK
.L0149 ;  playfield:

  ifconst pfres
    ldx #4*pfres-1
  else
	  ldx #47
  endif
	jmp pflabel8
PF_data8
	.byte %01111001, %11100111, %01110001, %00111011
	.byte %10000001, %01001000, %01001001, %01001000
	.byte %01111001, %01001000, %01000101, %00111011
	.byte %00000101, %01000111, %01001001, %01001000
	.byte %01111001, %11100000, %01110001, %01001011
	.byte %00000000, %00000000, %00000000, %00000000
	.byte %01001110, %00011100, %01100001, %01001000
	.byte %01001001, %00010010, %10010001, %01001001
	.byte %01001000, %10010001, %11111001, %01001010
	.byte %00001000, %10010000, %00001001, %00001100
	.byte %01001000, %10010000, %00001001, %01001000
	.byte %00000000, %00000000, %00000000, %00000000
pflabel8
	lda PF_data8,x
	sta playfield,x
	dex
	bpl pflabel8
.L0150 ;  drawscreen

 jsr drawscreen
.L0151 ;  if switchreset then score = 0  :  pfscore2 = 21  :  goto main

 lda #1
 bit SWCHB
	BNE .skipL0151
.condpart39
	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
	LDA #21
	STA pfscore2
 jmp .main

.skipL0151
.L0152 ;  rem ==========/LOGIC=========

 if (<*) > (<(*+16))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL064_0

	.byte 0
	.byte  %00011100
	.byte  %00111000
	.byte  %00011000
	.byte  %00111000
	.byte  %00111000
	.byte  %00011100
	.byte  %01011100
	.byte  %11011100
	.byte  %11111000
	.byte  %11100100
	.byte  %11100010
	.byte  %11101010
	.byte  %01100100
	.byte  %00111100
	.byte  %00010000
 if (<*) > (<(*+16))
	repeat ($100-<*)
	.byte 0
	repend
	endif
player6then_0

	.byte 0
	.byte  %00011100
	.byte  %00111000
	.byte  %00011000
	.byte  %00111000
	.byte  %00111000
	.byte  %00011100
	.byte  %01001100
	.byte  %11010111
	.byte  %11111000
	.byte  %11100100
	.byte  %11100010
	.byte  %11101010
	.byte  %01100100
	.byte  %00111100
	.byte  %00010000
 if (<*) > (<(*+13))
	repeat ($100-<*)
	.byte 0
	repend
	endif
player7then_1

	.byte 0
	.byte  %00011100
	.byte  %00001100
	.byte  %00011000
	.byte  %00011011
	.byte  %00001111
	.byte  %00010111
	.byte  %00010111
	.byte  %00111110
	.byte  %01100111
	.byte  %00100111
	.byte  %00110010
	.byte  %00011100
 if (<*) > (<(*+13))
	repeat ($100-<*)
	.byte 0
	repend
	endif
player8then_1

	.byte 0
	.byte  %00111000
	.byte  %00110000
	.byte  %00011000
	.byte  %11011000
	.byte  %11110000
	.byte  %11101000
	.byte  %11101000
	.byte  %01111100
	.byte  %11100110
	.byte  %11100100
	.byte  %01001100
	.byte  %00111000
 if (<*) > (<(*+4))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0119_1

	.byte 0
	.byte  %0111110
	.byte  %0101010
	.byte  %1010101
 if (<*) > (<(*+15))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0143_1

	.byte 0
	.byte  %00111000
	.byte  %00110000
	.byte  %00011000
	.byte  %11011000
	.byte  %11110000
	.byte  %11101000
	.byte  %11101000
	.byte  %01111000
	.byte  %01000100
	.byte  %01000010
	.byte  %11101010
	.byte  %11100100
	.byte  %01111100
	.byte  %00111000
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 
 
 
; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif


scoretable
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 


 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word start
 .word start
