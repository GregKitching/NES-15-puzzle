.include "nes2header.inc"
nes2mapper 0
nes2prg 32768
nes2chr 8192
;nes2chrram 8192
nes2mirror 'H'
nes2tv 'N'
nes2end

.segment "STARTUP"

.segment "VECTORS"
  .word nmi
  .word reset
  .word irq

.segment "CHARS"

chars:
  .incbin "15tile.chr"

.segment "CODE"

nametable:
  .incbin "15tile.nam"

;.segment "RODATA"

;mytiles_chr:
;  .incbin "15tile.chr"

;Variables
;zero page
;$00-$01: temp
;$02-$03: controller input
;$04-$05: temp variables for buttona routine
;$06-$07: array indices in $0300 of blocks to swap
;$08-$09: the numbers those blocks actually contain
;$10-$11: Nametable address of first block to swap
;$12-$13: Nametable address of second block to swap
;$14: Character to be swapped into first block
;$15: Character to be swapped into second block
;$16-$19: Copy of attribute table entries $23db, $23dc, $23e3, and $23e4 in nametable
;$1a: flag for nmi block swap operation (>$00 = swap)

;page $03
;$00 - 0f: tile number at position on board (row-major) (hole = 0)
;$10: x position of hole
;$11: y position of hole
;$12: x position of cursor ($00 - $03)
;$13: y position of cursor ($00 - $03)
;$14: button pressed flag
;$20: move sound counter

; Initial State
; 13 2  10 3
; 1  12 8  4
; 5  9  6  7
; 15 14 11

reset:
  sei
  cld
  ldx #$FF
  tsx
  inx
  lda #$00
  sta $2000
  sta $2001
  sta $4010
  sta $4015
  lda #$40
  sta $4017

  bit $2002; wait for ppu initialization
: bit $2002
  bpl :-
: bit $2002
  bpl :-
  jsr clearppumem
  ;jsr copypatterntable
  jsr copynametable
  jsr setpalettes
  jsr initializeapu
  jmp start

copypatterntable:
  lda #<chars
  sta $00
  lda #>chars
  sta $01
  bit $2002; Reset $2005/$2006 latch
  ldy #$00
  sty $2001; Disable rendering if not already disabled
  sty $2006; Load pattern table address
  sty $2006
  ldx #32; # of 256 byte pages to copy, 256 * 32 = 8192
  :lda ($00), y
  sta $2007
  iny
  bne :-
  inc $01
  dex
  bne :-
  rts

copynametable:
  lda #<nametable
  sta $00
  lda #>nametable
  sta $01
  bit $2002
  ldy #$20
  sty $2006
  ldy #$00
  sty $2006
  ldx #$04
  :lda ($00), y
  sta $2007
  iny
  bne :-
  inc $01
  dex
  bne :-
  rts

setpalettes:
  bit $2002
  lda #$3f ; load memory address of palette area in vram
  sta $2006
  lda #$00
  sta $2006
  lda #$0f ; black
  sta $2007
  lda #$00
  sta $2007
  lda #$10
  sta $2007
  lda #$30
  sta $2007
  sta $2007
  lda #$11
  sta $2007
  lda #$21
  sta $2007
  lda #$30
  sta $2007
  sta $2007
  lda #$06
  sta $2007
  lda #$16
  sta $2007
  lda #$30
  sta $2007
  sta $2007
  lda #$0a
  sta $2007
  lda #$1a
  sta $2007
  lda #$2a
  sta $2007
  lda #$0f; Writing here ($3f10) also seems to set the universal background colour
  sta $2007
  lda #$0a
  sta $2007
  lda #$1a
  sta $2007
  lda #$2a
  sta $2007
  rts

initializeapu:
  lda #%00110000
  sta $4000
  sta $4004
  sta $400c
  lda #%10000000
  sta $4008
  ;sta $4017
  lda #%00001111
  sta $4015
  lda #%01000000
  sta $4017
  rts

setattributetable:
  lda #$23
  sta $2006
  lda #$c0
  sta $2006
  lda #%11100100
  ldx #$00
: sta $2007
  inx
  cpx #64
  bne :-
  sta $2007
  rts
  
clear256:
  tax
: sta $2007
  inx
  cpx #$ff
  bne :-
  sta $2007
  rts

clearppumem:
  lda #$20
  sta $2006
  lda #$00
  sta $2006
  jsr clear256
  jsr clear256
  jsr clear256
  tax
: sta $2007
  inx
  cpx #$bf
  bne :-
  sta $2007
  rts

setppumem:
  lda #$20
  sta $2006
  lda #$00
  sta $2006
  tax
: stx $2007
  inx
  cpx #$ff
  bne :-
  stx $2007
  rts

clearoamcopy:
  lda #$02
  sta $01
  lda #$00
  tay
  sta $00
  ldx #$2e; Index of blank tile
: sta ($00), Y
  iny
  txa
  sta ($00), Y; Set tile index to blank tile but all other fields to $00
  iny
  lda #$00
  sta ($00), Y
  iny
  sta ($00), Y
  iny
  cpy #$00
  bne :-
  ;sta ($00), Y
  rts

setoamcopy:
  lda #87;#71
  sta $0204; Sprite 1 (save sprite 0 for potential sprite 0 hit functionality)
  sta $0208; Sprite 2
  lda #111;#95
  sta $020c; Sprite 3
  sta $0210; Sprite 4
  lda #$44
  sta $0205
  sta $0209
  sta $020d
  sta $0211
  lda #%00000000
  sta $0206
  lda #%01000000; flip horizontally
  sta $020a
  lda #%10000000; flip vertically
  sta $020e
  lda #%11000000
  sta $0212
  lda #88
  sta $0207
  sta $020f
  lda #112
  sta $020b
  sta $0213
  rts

initvars:
  lda #13
  sta $0300
  lda #2
  sta $0301
  lda #10
  sta $0302
  lda #3
  sta $0303
  sta $0310
  sta $0311
  lda #1
  sta $0304
  lda #12
  sta $0305
  lda #8
  sta $0306
  lda #4
  sta $0307
  lda #5
  sta $0308
  lda #9
  sta $0309
  lda #6
  sta $030a
  lda #7
  sta $030b
  lda #15
  sta $030c
  lda #14
  sta $030d
  lda #11
  sta $030e
  lda #0
  sta $030f
  sta $0312
  sta $0313
  sta $10
  sta $0314
  sta $02
  sta $1a
  ldx #$db
  lda $8300, x
  sta $16
  inx
  lda $8300, x
  sta $17
  ldx #$e3
  lda $8300, x
  sta $18
  inx
  lda $8300, x
  sta $19
  lda #15
  jsr gethole2dindex
  stx $0310
  sty $0311
  rts

gettoplefttile:
  cmp #$00
  bne :+
  lda #$2e
  rts
 :sec
  sbc #$01
  clc
  rol
  cmp #$10
  bcc :+ ;branch if A < $10
  clc
  adc #$10
 :rts

fillnametablespace:
  lda #$21
  sta $2006
  lda #$8c
  sta $2006
  ldx #$00
 :lda $0300, x
  jsr gettoplefttile
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  lda #$21
  sta $2006
  lda #$ac
  sta $2006
  ldx #$00
 :lda $0300, x
  jsr gettoplefttile
  clc
  adc #$10
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  
  lda #$21
  sta $2006
  lda #$cc
  sta $2006
  ldx #$00
 :lda $0304, x
  jsr gettoplefttile
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  lda #$21
  sta $2006
  lda #$ec
  sta $2006
  ldx #$00
 :lda $0304, x
  jsr gettoplefttile
  clc
  adc #$10
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  
  lda #$22
  sta $2006
  lda #$0c
  sta $2006
  ldx #$00
 :lda $0308, x
  jsr gettoplefttile
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  lda #$22
  sta $2006
  lda #$2c
  sta $2006
  ldx #$00
 :lda $0308, x
  jsr gettoplefttile
  clc
  adc #$10
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  
  lda #$22
  sta $2006
  lda #$4c
  sta $2006
  ldx #$00
 :lda $030c, x
  jsr gettoplefttile
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  lda #$22
  sta $2006
  lda #$6c
  sta $2006
  ldx #$00
 :lda $030c, x
  jsr gettoplefttile
  clc
  adc #$10
  sta $2007
  clc
  adc #$01
  sta $2007
  inx
  cpx #$04
  bcc :-
  
  rts

getcolour:
  cmp #$00
  bne :+
  lda #$00
  rts
 :clc
  ror
  bcs :+
  lda #%00000001
  rts
 :lda #%00000010
  rts

fillattributetablespace:
  lda #$00
  sta $1b
  lda $0300
  jsr getcolour
  ora $1b
  sta $1b
  lda $0301
  jsr getcolour
  jsr mul4
  ora $1b
  sta $1b
  lda $0304
  jsr getcolour
  jsr mul16
  ora $1b
  sta $1b
  lda $0305
  jsr getcolour
  jsr mul64
  ora $1b
  sta $1b
  sta $16
  lda #$23
  sta $2006
  lda #$db
  sta $2006
  lda $1b
  sta $2007
  
  lda #$00
  sta $1b
  lda $0302
  jsr getcolour
  ora $1b
  sta $1b
  lda $0303
  jsr getcolour
  jsr mul4
  ora $1b
  sta $1b
  lda $0306
  jsr getcolour
  jsr mul16
  ora $1b
  sta $1b
  lda $0307
  jsr getcolour
  jsr mul64
  ora $1b
  sta $1b
  sta $17
  sta $2007
  
  lda #$00
  sta $1b
  lda $0308
  jsr getcolour
  ora $1b
  sta $1b
  lda $0309
  jsr getcolour
  jsr mul4
  ora $1b
  sta $1b
  lda $030c
  jsr getcolour
  jsr mul16
  ora $1b
  sta $1b
  lda $030d
  jsr getcolour
  jsr mul64
  ora $1b
  sta $1b
  sta $18
  lda #$23
  sta $2006
  lda #$e3
  sta $2006
  lda $1b
  sta $2007
  
  lda #$00
  sta $1b
  lda $030a
  jsr getcolour
  ora $1b
  sta $1b
  lda $030b
  jsr getcolour
  jsr mul4
  ora $1b
  sta $1b
  lda $030e
  jsr getcolour
  jsr mul16
  ora $1b
  sta $1b
  lda $030f
  jsr getcolour
  jsr mul64
  ora $1b
  sta $1b
  sta $19
  sta $2007
  
  rts

test:
  lda #$21
  sta $10
  sta $12
  lda #$50
  sta $11
  lda #$90
  sta $13
  lda #$08
  sta $14
  lda #$24
  sta $15
  lda #$d4
  sta $16
  lda #$dc
  sta $17
  lda #%10100000
  sta $18
  lda #%10010110
  sta $19
  sta $1a
  rts

start:
  jsr clearoamcopy
  jsr setoamcopy
  jsr initvars
  jsr fillnametablespace
  jsr fillattributetablespace
  ;jsr test
  lda $00; set scroll
  sta $2005
  sta $2005
  lda #%10000000; Enable nmi
  sta $2000
  lda #%00011110; Show background and sprites
  sta $2001
loop:
  lda $1a
  bne loop
: jsr readcontroller
  lda $02
  sta $03
  jsr readcontroller; Read controller twice and compare due to DPCM conflict (not using DPCM right now, but good practice)
  lda $02
  cmp $03
  bne :-
  lda $02
  tax
  and #$01
  beq :+
  jmp buttonright
: txa
  and #$02
  beq :+
  jmp buttonleft
: txa
  and #$04
  beq :+
  jmp buttondown
: txa
  and #$08
  beq :+
  jmp buttonup
: txa
  and #$80
  beq :+
  jmp buttona
: lda #$00
  sta $0314
  jmp loop

readcontroller:; Adapted from https://www.nesdev.org/wiki/Controller_reading_code
  lda #$01
  sta $4016
  sta $02; storing controller state vector here
  lsr a
  sta $4016
: lda $4016
  lsr a
  rol $02
  bcc :-
  rts

; Button assignment: 7   6     5        4     3     2      1      0
;                    A | B | Select | Start | Up | Down | Left | Right

buttonright:
  lda $0314
  bne :+; abort if disable input is set
  lda $0312
  cmp #$03
  bcs :+; abort if xpos >= #$03
  inc $0312
  lda #$01
  sta $0314
  jsr movecursor
  jsr playmovenote
  :jmp loop

buttonleft:
  lda $0314
  bne :+
  lda $0312
  beq :+
  dec $0312
  lda #$01
  sta $0314
  jsr movecursor
  jsr playmovenote
  :jmp loop

buttondown:
  lda $0314
  bne :+
  lda $0313
  cmp #$03
  bcs :+
  inc $0313
  lda #$01
  sta $0314
  jsr movecursor
  jsr playmovenote
  :jmp loop

buttonup:
  lda $0314
  bne :+
  lda $0313
  beq :+
  dec $0313
  lda #$01
  sta $0314
  jsr movecursor
  jsr playmovenote
  :jmp loop

buttona:
; if $0312 - $0310 == #$00
;   if $0313 - $0311 == #$01
;     hole is above cursor
;   else if $0313 - $0311 == #$ff
;     hole is below cursor
;   else
;     do nothing
; else if $0312 - $0310 == #$01
;   if $0313 - $0311 == #$00
;     hole is to the left of cursor
;   else
;     do nothing
; else if $0312 - $0310 == #$ff
;   if $0313 - $0311 == #$00
;     hole is to the right of cursor
;   else
;     do nothing
; else
;   do nothing
  lda $0312
  sec
  sbc $0310
  sta $04
  lda $0313
  sec
  sbc $0311
  sta $05
  lda $04
  bne xnotzero
  lda $05
  cmp #$01
  bne ynotone
  jmp swaphole
ynotone:
  cmp #$ff
  bne exit
  jmp swaphole
xnotzero:
  cmp #$01
  bne xnotone
  lda $05
  bne exit
  jmp swaphole
xnotone:
  cmp #$ff
  bne exit
  lda $05
  bne exit
  jmp swaphole
exit:
  jmp loop

playmovenote:
  lda #$32
  sta $0320
  lda #$01
  sta $4015
  lda #$fd
  sta $4002
  lda #$00
  sta $4003
  lda #%10001111
  sta $4000
  rts

playswapnote:
  lda #$04
  sta $0320
  lda #%00001000
  sta $4015
  lda #%10001111
  sta $400e
  lda #$00
  sta $400f
  lda #%00111111
  sta $400c
  rts

swaphole:
  jsr getarrayindexcursor
  sta $06
  tax
;swap numbers in $0300 array
  lda $0300, x
  sta $08
  jsr getarrayindexhole
  sta $07
  tax
  lda $0300, x
  sta $09
  ldx $06
  sta $0300, x
  lda $08
  ldx $07
  sta $0300, x
;swap blocks in nametable
  ldx $06
  lda nametableaddresslsb, x
  sta $11
  jsr getnametableaddressmsb
  sta $10
  ldx $07
  lda nametableaddresslsb, x
  sta $13
  jsr getnametableaddressmsb
  sta $12
  lda #$2e
  sta $14
  ldx $08
  lda charpos, x
  sta $15
  lda $0312; update position of hole
  sta $0310
  lda $0313
  sta $0311
  jsr swapattributetables
  lda #$01; set swap flag
  sta $1a
  jsr playswapnote
  jmp loop

;$00 -> $16 xxxxxx..
;$01 -> $16 xxxx..xx
;$02 -> $17 xxxxxx..
;$03 -> $17 xxxx..xx
;$04 -> $16 xx..xxxx
;$05 -> $16 ..xxxxxx
;$06 -> $17 xx..xxxx
;$07 -> $17 ..xxxxxx
;$08 -> $18 xxxxxx..
;$09 -> $18 xxxx..xx
;$0a -> $19 xxxxxx..
;$0b -> $19 xxxx..xx
;$0c -> $18 xx..xxxx
;$0d -> $18 ..xxxxxx
;$0e -> $19 xx..xxxx
;$0f -> $19 ..xxxxxx

; x | x | x | x | position bit 1 | 0 = xxxx.... | position bit 0 | 0 = xx..xx..
;                                  1 = ....xxxx                    1 = ..xx..xx

;$20: Position of first attribute to be changed
;$21: Position of second attribute to be changed
;$22: Position of bits in first attribute         00 = xxxxxx.. | 01 = xxxx..xx | 10 = xx..xxxx | 11 = ..xxxxxx
;$23: Position of bits in second attribute
;$24: Temp

swapattributetables:
  ldx #$00
  jsr findattributeentry
  jsr findbyteposition
  inx
  jsr findattributeentry
  jsr findbyteposition
  ldx $20
  lda $16, x
  ldy $22
  bne :+
  and #%00000011
  jmp cont
 :dey
  bne :+
  and #%00001100
  jsr div4
  jmp cont
 :dey
  bne :+
  and #%00110000
  jsr div16
  jmp cont
 :and #%11000000
  jsr div64
cont:
  sta $24
  ldx $21
  lda $16, x
  ldy $23
  bne :+
  and #%00000011
  jmp cont2
 :dey
  bne :+
  and #%00001100
  jsr div4
  jmp cont2
 :dey
  bne :+
  and #%00110000
  jsr div16
  jmp cont2
 :and #%11000000
  jsr div64
cont2:
  sta $25
  ldx $20
  lda $16, x
  ldy $22
  bne :+
  and #%11111100
  ora $25
  jmp cont3
 :dey
  bne :+
  and #%11110011
  clc
  rol $25
  rol $25
  ora $25
  jmp cont3
 :dey
  bne :+
  and #%11001111
  clc
  rol $25
  rol $25
  rol $25
  rol $25
  ora $25
  jmp cont3
 :and #%00111111
  clc
  rol $25
  rol $25
  rol $25
  rol $25
  rol $25
  rol $25
  ora $25
cont3:
  sta $16, x
  ldx $21
  lda $16, x
  ldy $23
  bne :+
  and #%11111100
  ora $24
  jmp cont4
 :dey
  bne :+
  and #%11110011
  clc
  rol $24
  rol $24
  ora $24
  jmp cont4
 :dey
  bne :+
  and #%11001111
  clc
  rol $24
  rol $24
  rol $24
  rol $24
  ora $24
  jmp cont4
 :and #%00111111
  clc
  rol $24
  rol $24
  rol $24
  rol $24
  rol $24
  rol $24
  ora $24
cont4:
  sta $16, x
  rts

;findattributelsb:
;  lda $06, x
;  jsr mul32; shift left by five
;  bcs ezero
;  lda #$d0
;  sta $16, x
;  jsr mul4
;  bcs :+
;  lda $16, x
;  ora #$0b
;  sta $16, x
;  rts
; :lda $16, x
;  ora #$0c
;  sta $16, x
;  rts
;ezero:
;  lda #$e0
;  sta $16, x
;  jsr mul4
;  bcs :+
;  lda $16, x
;  ora #$03
;  sta $16, x
;  rts
; :lda $16, x
;  ora #$04
;  sta $16, x
;  rts

findattributeentry:
  lda $06, x
  and #%00001010
  lsr a
  lsr a
  adc #$00; increment A by one if a 1 was shifted into carry
  sta $20, x
  rts

findbyteposition:
  lda $06, x
  and #%00000101
  lsr a
  adc #$00
  sta $22, x
  rts

getarrayindexcursor:; For translating cursor x and y position into an index into the 1d array, store result in A
  lda $0313
  jsr mul4
  ora $0312
  rts

getarrayindexhole:
  lda $0311
  jsr mul4
  ora $0310
  rts

gethole2dindex:; Inverse of getarrayindex; have index in A, x pos -> X, y pos -> Y
  tay
  and #$03
  tax
  tya
  lsr
  lsr
  and #$03
  tay
  rts

getnametableaddressmsb:
; $218c | $218e | $2190 | $2192
;-------+-------+-------+-------
; $21cc | $21ce | $21d0 | $21d2
;-------+-------+-------+-------
; $220c | $220e | $2210 | $2212
;-------+-------+-------+-------
; $224c | $224e | $2250 | $2252
  cmp #$80
  bcs :+
  lda #$22
  rts
 :lda #$21
  rts

nametableaddresslsb:
.byte $8c, $8e, $90, $92, $cc, $ce, $d0, $d2, $0c, $0e, $10, $12, $4c, $4e, $50, $52

charpos:
.byte $2e, $00, $02, $04, $06, $08, $0a, $0c, $0e, $20, $22, $24, $26, $28, $2a, $2c

movecursor:
  lda $0312
  jsr mul16
  adc #88
  tax
  lda $0313
  jsr mul16
  adc #87;71
  tay
  stx $0207
  stx $020f
  sty $0204
  sty $0208
  txa
  adc #24
  tax
  tya
  adc #24
  tay
  stx $020b
  stx $0213
  sty $020c
  sty $0210
  rts
  
mul64:
  clc
  rol a
mul32:
  clc
  rol a
mul16:
  clc
  rol a
mul8:
  clc
  rol a
mul4:
  clc
  rol a
mul2:
  clc
  rol a
  rts

div64:
  clc
  ror a
div32:
  clc
  ror a
div16:
  clc
  ror a
div8:
  clc
  ror a
div4:
  clc
  ror a
div2:
  clc
  ror a
  rts

movenotelogic:
  lda $0320
  beq :+
  dec $0320
  bne :+
  lda #%00110000
  sta $4000
  lda #$00
  sta $4015
 :rts

nmi:
  php
  pha
  txa
  pha
  tya
  pha
  bit $2002
  lda #$00
  sta $2003
  lda #$02
  sta $4014
  lda $1a
  bne :+
  jmp skiptileswap
;tile swapping routine
 :lda $10; X = tile number, Y = lsb of nametable address
  sta $2006
  lda $11
  tay
  sta $2006
  ldx $14
  stx $2007
  inx
  stx $2007
  txa
  clc
  adc #$0f
  tax
  tya
  clc
  adc #$20
  tay
  lda $10
  sta $2006
  sty $2006
  stx $2007
  inx
  stx $2007
;second tile
  lda $12;
  sta $2006
  lda $13
  tay
  sta $2006
  ldx $15
  stx $2007
  inx
  stx $2007
  txa
  clc
  adc #$0f
  tax
  tya
  clc
  adc #$20
  tay
  lda $12
  sta $2006
  sty $2006
  stx $2007
  inx
  stx $2007
;first attribute table entry
;  lda #$23
;  ldx $16
;  ldy $18
;  sta $2006
;  stx $2006
;  sty $2007
;second attribute table entry
;  ldx $17
;  ldy $19
;  sta $2006
;  stx $2006
;  sty $2007
;  lda #$00; Reset scroll register due to shared internal register with $2006
;  sta $2005
;  sta $2005
  ldx #$23
  ldy #$db
  stx $2006
  sty $2006
  lda $16
  sta $2007
  lda $17
  sta $2007
  ldy #$e3
  stx $2006
  sty $2006
  lda $18
  sta $2007
  lda $19
  sta $2007
  lda #$00; Reset scroll register due to shared internal register with $2006
  sta $2005
  sta $2005
  sta $1a; Clear input lock
skiptileswap:
  jsr movenotelogic
  pla
  tay
  pla
  tax
  pla
  plp
  rti

irq:
  rti
