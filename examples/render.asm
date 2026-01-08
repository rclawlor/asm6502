; NES header
.ineschr 1
.inesprg 1
.inesmir 0
.inesmap 0


.org $C000

Reset:
    LDX #$02
WarmUp:
    BIT $2002
    BPL WarmUp
    DEX
    BNE WarmUp  ; Loop until X=0

    SEI
    CLD         ; Clear decimal mode
    LDX #$00
    STX $2000   ; Disable NMI
    STX $2001   ; Disable sprites/background
    DEX
    TXS
    LDA #$00
    TAX
ClearRAM:
    STA $0000, X
    STA $0100, X
    STA $0200, X
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X

    INX
    BNE ClearRAM

    LDX #$03
Boot:
    BIT $2002
    BPL Boot
    DEX
    BNE Boot
    
LoadPalette:
    LDA #$3F        ; Set PPU write address to $3F00
    STA $2006
    LDX #$00
    STX $2006

PaletteLoop:
    LDA Palette, X
    STA $2007
    INX
    CMP #$10
    BNE PaletteLoop

Exit:
    LDA #%10001000      ; Turn on NMI
    STA $2000
    LDA #%00011110      ; Turn on screen
    STA $2001

    LDA #$00
    STA $2005           ; PPU scroll X
    STA $2005           ; PPU scroll Y

Loop:
    JMP Loop


NMI:
    PHA
    TXA
    PHA
    TYA
    PHA         ; Backup A, X and Y to the stack

    LDX #$00
    STX $2001   ; Disable sprites/background

    LDA $2002   ; Clear high/low latch
    LDA #$20    ; Load high byte of screen address
    STA $2006   ; Set PPU write address
    LDA #$50    ; Load low byte of screen address
    STA $2006

    LDA #%10001000  ; Turn on NMI
    STA $2000
    LDA #%00011110  ; Turn on screen
    STA $2001
    LDA #$00
    STA $2005
    STA $2006

    PLA
    TAY
    PLA
    TAX
    PLA

    RTI

IRQ:
    RTI

Palette:
    ; Background palette data
    .db $0F,$31,$32,$33,$0F,$35,$36,$37,$0F,$39,$3A,$3B,$0F,$3D,$3E,$0F
    ; Sprite palette data
    .db $0F,$1C,$15,$14,$0F,$02,$38,$3C,$0F,$1C,$15,$14,$0F,$02,$38,$3C

.org $FFFA

.dw NMI
.dw Reset
.dw IRQ
