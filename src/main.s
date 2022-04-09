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
  lda #71
  sta $0204; Sprite 1 (save sprite 0 for potential sprite 0 hit functionality)
  sta $0208; Sprite 2
  lda #95
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

;Variables
;zero page
;$10-$11: Nametable address of first block to swap
;$12-$13: Nametable address of second block to swap
;$14: Character to be swapped into first block
;$15: Character to be swapped into second block
;$16: lsB of first attribute table entry to be changed
;$17: lsB of second attribute table entry to be changed
;$18: new value of first attribute table entry
;$19: new value of second attribute table entry
;$1a: flag for nmi block swap operation (>$00 = swap)

;page $03
;$00 - 0f: tile number at position on board (row-major) (hole = 0)
;$10: position of hole
;$11: x position of cursor ($00 - $03)
;$12: y position of cursor ($00 - $03)
;$13: button pressed flag

initvars:
  lda #13
  sta $0300
  lda #2
  sta $0301
  lda #10
  sta $0302
  lda #3
  sta $0303
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
  sta $0310
  lda #14
  sta $030d
  lda #11
  sta $030e
  lda #0
  sta $030f
  sta $0311
  sta $0312
  sta $10
  sta $0313
  sta $02
  sta $1a
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
  :jsr readcontroller
  lda $02
  sta $03
  jsr readcontroller; Read controller twice and compare due to DPCM conflict (not using DPCM right now, but good practice)
  lda $02
  cmp $03
  bne :-
  lda $02
  tax
  and #$01
  bne buttonright
  txa
  and #$02
  bne buttonleft
  txa
  and #$04
  bne buttondown
  txa
  and #$08
  bne buttonup
  txa
  and #$80
  ;bne buttona
  lda #$00
  sta $0313
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

; Button assignment:	7   6     5        4     3     2      1      0
;			A | B | Select | Start | Up | Down | Left | Right

buttonright:
  lda $0313
  bne :+; abort if disable input is set
  lda $0311
  cmp #$03
  bcs :+; abort if xpos >= #$03
  inc $0311
  lda #$01
  sta $0313
  jsr movecursor
  :jmp loop

buttonleft:
  lda $0313
  bne :+
  lda $0311
  beq :+
  dec $0311
  lda #$01
  sta $0313
  jsr movecursor
  :jmp loop

buttondown:
  lda $0313
  bne :+
  lda $0312
  cmp #$03
  bcs :+
  inc $0312
  lda #$01
  sta $0313
  jsr movecursor
  :jmp loop

buttonup:
  lda $0313
  bne :+
  lda $0312
  beq :+
  dec $0312
  lda #$01
  sta $0313
  jsr movecursor
  :jmp loop

getarrayindex:; For translating cursor x and y position into an index into the 1d array, store result in A
  lda $0312
  jsr mul4
  ora $0311
  rts

movecursor:
  lda $0311
  jsr mul16
  adc #88
  tax
  lda $0312
  jsr mul16
  adc #71
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
  beq skiptileswap
;tile swapping routine
  lda $10; X = tile number, Y = lsb of nametable address
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
  lda #$23
  ldx $16
  ldy $18
  sta $2006
  stx $2006
  sty $2007
;second attribute table entry
  ldx $17
  ldy $19
  sta $2006
  stx $2006
  sty $2007
  lda #$00; Reset scroll register due to shared internal register with $2006
  sta $2005
  sta $2005
skiptileswap:
  lda #$00
  sta $1a; Clear input lock
  pla
  tay
  pla
  tax
  pla
  plp
  rti

irq:
  rti
