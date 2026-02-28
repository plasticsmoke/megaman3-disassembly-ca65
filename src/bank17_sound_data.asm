; =============================================================================
; MEGA MAN 3 (U) — BANK $17 — SOUND/MUSIC DATA
; =============================================================================
; Mapped to $A000-$BFFF. Always loaded alongside bank $16 ($8000-$9FFF)
; which contains the sound driver code. Together they form the audio engine.
;
; Bank $16 code calls `read_ptr` to access this data, with high-byte
; remapping: addresses $A000-$BFFF read from this bank directly, while
; addresses $C000+ temporarily swap in bank $18 to read from there.
;
; Contains music sequence data and sound effect definitions for the NES APU
; driver. The sound pointer table in bank $16 at $8A44 indexes all sound IDs.
; Sound IDs $00-$04 (Title Screen through Hard Man) have data in bank $16
; ($8C9D-$9FFF); IDs $05-$11 continue into this bank ($A000-$BFFF).
; ID $12 (Ending) and SFX/jingles ($13+) point to bank $18 via read_ptr.
; Each track consists of channel sub-sequences with note data, duration,
; instrument selection, and loop markers.
;
; Music track pointers (interleaved table in bank $16 at $8A44):
;   Track $05 → $A0BE    Track $06 → $A414    Track $07 → $A7C1
;   Track $08 → $AAC2    Track $09 → $ADE5    Track $0A → $B123
;   Track $0B → $B2CB    Track $0C → $B416    Track $0D → $B5F6
;   Track $0E → $B82B    Track $0F → $B91E    Track $10 → $BE97
;   Track $11 → $BFCF
;
; Called every NMI frame via play_sounds in the fixed bank, which swaps
; banks $16/$17 into the $8000-$BFFF window.
; =============================================================================

        .setcpu "6502"

.include "include/zeropage.inc"
.include "include/constants.inc"
.include "include/hardware.inc"


.segment "BANK17"

; =============================================================================
; MUSIC SEQUENCE DATA ($A000-$BFFF)
; =============================================================================
; Continuation of music/SFX data from bank $16. Each track contains
; per-channel sub-sequences with NES APU register commands, note events
; (pitch + duration), instrument parameters, volume envelopes, and loop
; control bytes. The sound driver reads this data sequentially per channel,
; advancing a pointer each frame.
;
; No fixed internal boundaries — track data flows continuously from $A000
; to $BFFF as addressed by the bank $16 pointer table.
; =============================================================================

        .byte   $09,$03,$08,$02,$64,$60,$64,$64 ; cmd: set detune $03
        .byte   $6D,$70,$60,$64,$60,$64,$60,$64 ; note data w/ rests
        .byte   $64,$60,$64,$64,$0E,$01,$9F,$FE ; cmd: loop/jmp G0 ct=$01 → $9FFE
        .byte   $04,$00,$69,$68,$69,$60,$75,$69 ; cmd: NOP $00
        .byte   $60,$6B,$60,$6B,$60,$68,$77,$60 ; note data w/ rests
        .byte   $6B,$60,$6D,$6B,$6D,$60,$79,$6D ; note data w/ rests
        .byte   $60,$6D,$12,$00,$A0,$42,$60,$6D ; cmd: loop end G0 fl=$00 → $A042
        .byte   $60,$6B,$79,$60,$6D,$60,$0E,$03 ; cmd: loop/jmp G0 ct=$03 → $A018
        .byte   $A0,$18,$60,$6E,$6E,$60,$6E,$60 ; note data w/ rests
        .byte   $6E,$6E,$16,$9F,$57,$17,$04,$00 ; --- Bank16 SID4 overflow ch0 (Triangle) ---
        .byte   $06,$C8,$07,$0A,$08,$0C,$04,$00 ; cmd: set dur mul $C8
        .byte   $04,$00,$63,$6F,$63,$6F,$68,$63 ; cmd: NOP $00
        .byte   $63,$63,$6F,$63,$6F,$63,$68,$6F ; note data
        .byte   $6F,$6D,$0E,$07,$A0,$58,$14,$00 ; cmd: loop/jmp G0 ct=$07 → $A058
        .byte   $A0,$9A,$04,$00,$62,$6F,$6F,$62 ; cmd: NOP $00
        .byte   $68,$6F,$62,$6F,$60,$6F,$62,$6F ; note data w/ rests
        .byte   $68,$6F,$6F,$60,$0E,$07,$A0,$72 ; cmd: loop/jmp G0 ct=$07 → $A072
        .byte   $04,$00,$62,$62,$62,$62,$66,$65 ; cmd: NOP $00
        .byte   $63,$62,$0E,$03,$A0,$88,$10,$01 ; cmd: loop/jmp G0 ct=$03 → $A088
        .byte   $A0,$56,$63,$6F,$63,$6F,$68,$63 ; note data w/ rests
        .byte   $63,$63,$6F,$63,$6F,$63,$68,$6F ; note data
        .byte   $6F,$6F,$63,$6F,$63,$6F,$68,$63 ; note data
        .byte   $63,$63,$6F,$68,$68,$60,$68,$60 ; note data w/ rests
; --- Track $05: Top Man stage ($A0BE) ---
        .byte   $68,$68,$16,$A0,$4E,$17,$00,$A0 ; Track $05 header: $00=music (Top Man stage)
        .byte   $C7,$A1,$D1,$A2,$E1,$A3,$80,$0A ; ch3 ptr lo → $A0C7
        .byte   $FD,$05,$02,$66,$06,$E6,$07,$09 ; cmd: set tempo $02,$66
        .byte   $18,$40,$08,$1D,$09,$02,$04,$08 ; cmd: set duty 25%
        .byte   $89,$88,$86,$02,$AB,$89,$88,$86 ; cmd: set dotted note
        .byte   $02,$AB,$8C,$8D,$12,$08,$A0,$EE ; cmd: set dotted note
        .byte   $86,$84,$0E,$01,$A0,$D6,$89,$8A ; cmd: loop/jmp G0 ct=$01 → $A0D6
        .byte   $04,$00,$07,$09,$18,$40,$06,$D2 ; cmd: NOP $00
        .byte   $04,$00,$9E,$9E,$03,$8D,$86,$8B ; cmd: NOP $00
        .byte   $86,$89,$86,$84,$86,$89,$86,$8B ; note data
        .byte   $86,$12,$08,$A1,$14,$89,$01,$86 ; cmd: loop end G0 fl=$08 → $A114
        .byte   $0E,$01,$A0,$F8,$89,$01,$84,$01 ; cmd: loop/jmp G0 ct=$01 → $A0F8
        .byte   $84,$84,$8B,$84,$89,$84,$88,$01 ; cmd: toggle bit 6
        .byte   $84,$01,$84,$84,$88,$84,$89,$84 ; cmd: toggle bit 6
        .byte   $88,$01,$84,$01,$84,$84,$8B,$84 ; cmd: toggle bit 6
        .byte   $89,$84,$88,$84,$84,$85,$80,$86 ; note data w/ rests
        .byte   $80,$89,$88,$01,$86,$01,$86,$86 ; cmd: toggle bit 6
        .byte   $8D,$86,$8B,$86,$89,$01,$86,$01 ; cmd: toggle bit 6
        .byte   $86,$86,$8D,$86,$8B,$86,$89,$86 ; note data
        .byte   $04,$00,$07,$0A,$18,$80,$08,$1E ; cmd: NOP $00
        .byte   $06,$E6,$0D,$00,$BE,$03,$A8,$A9 ; cmd: set dur mul $E6
        .byte   $B0,$AF,$AB,$AB,$08,$1F,$01,$8F ; cmd: set pitch ofs $1F
        .byte   $0D,$3C,$8D,$02,$01,$CD,$8B,$01 ; cmd: loop start G3 ct=$3C
        .byte   $8D,$01,$ED,$08,$1E,$0D,$00,$A6 ; cmd: toggle bit 6
        .byte   $A8,$A9,$B0,$B2,$B0,$AF,$08,$1F ; cmd: set pitch ofs $1F
        .byte   $01,$8B,$0D,$3C,$8D,$02,$01,$CD ; cmd: toggle bit 6
        .byte   $8B,$01,$8D,$01,$ED,$0E,$01,$A1 ; cmd: toggle bit 6
        .byte   $50,$04,$00,$08,$1E,$0D,$00,$A0 ; cmd: NOP $00
        .byte   $03,$89,$80,$89,$88,$80,$89,$80 ; cmd: toggle bit 3
        .byte   $89,$80,$01,$C8,$01,$88,$0E,$01 ; cmd: toggle bit 6
        .byte   $A1,$91,$08,$1D,$86,$80,$02,$A6 ; cmd: set pitch ofs $1D
        .byte   $84,$86,$01,$89,$01,$89,$84,$86 ; cmd: toggle bit 6
        .byte   $A9,$84,$86,$89,$86,$80,$02,$A6 ; cmd: set dotted note
        .byte   $84,$86,$01,$89,$01,$89,$82,$80 ; cmd: toggle bit 6
        .byte   $83,$80,$84,$80,$85,$16,$A0,$F0 ; cmd: end channel
        .byte   $17,$06,$E6,$07,$09,$18,$40,$08 ; --- Track $05: Top Man stage ch2 (Pulse 1) ---
        .byte   $1D,$09,$02,$0C,$03,$04,$00,$95 ; cmd: set detune $02
        .byte   $94,$92,$02,$B7,$95,$94,$92,$02 ; cmd: set dotted note
        .byte   $B7,$98,$99,$12,$00,$A1,$F5,$92 ; cmd: loop end G0 fl=$00 → $A1F5
        .byte   $90,$0E,$01,$A1,$DD,$95,$96,$04 ; cmd: loop/jmp G0 ct=$01 → $A1DD
        .byte   $00,$18,$40,$0C,$00,$06,$D2,$08 ; cmd: set duty 25%
        .byte   $1D,$04,$00,$07,$09,$09,$02,$99 ; cmd: NOP $00
        .byte   $99,$9E,$99,$9C,$99,$9A,$99,$97 ; note data
        .byte   $99,$9C,$99,$9E,$99,$12,$00,$A2 ; cmd: loop end G0 fl=$00 → $A220
        .byte   $20,$9C,$01,$99,$0E,$01,$A2,$01 ; cmd: toggle bit 6
        .byte   $9C,$01,$97,$01,$97,$97,$9C,$97 ; cmd: toggle bit 6
        .byte   $9A,$97,$99,$97,$95,$97,$99,$97 ; note data
        .byte   $9A,$97,$99,$01,$97,$01,$97,$97 ; cmd: toggle bit 6
        .byte   $9C,$97,$9A,$97,$99,$97,$0C,$03 ; cmd: loop start G2 ct=$03
        .byte   $09,$02,$90,$91,$80,$92,$80,$95 ; cmd: set detune $02
        .byte   $94,$01,$92,$04,$00,$92,$92,$99 ; cmd: toggle bit 6
        .byte   $92,$97,$92,$95,$01,$92,$0E,$01 ; cmd: toggle bit 6
        .byte   $A2,$4B,$0C,$00,$04,$00,$04,$00 ; cmd: loop start G2 ct=$00
        .byte   $0D,$00,$92,$92,$99,$92,$97,$92 ; cmd: loop start G3 ct=$00
        .byte   $95,$92,$0E,$01,$A2,$5E,$92,$92 ; cmd: loop/jmp G0 ct=$01 → $A25E
        .byte   $03,$8B,$80,$01,$8B,$0D,$1E,$01 ; cmd: toggle bit 3
        .byte   $A9,$0D,$00,$01,$86,$01,$86,$86 ; cmd: loop start G3 ct=$00
        .byte   $8B,$80,$0D,$1E,$01,$8B,$13,$48 ; cmd: loop start G3 ct=$1E
        .byte   $A2,$91,$02,$01,$A9,$0F,$03,$A2 ; cmd: set dotted note
        .byte   $5C,$04,$00,$0D,$00,$92,$91,$01 ; cmd: NOP $00
        .byte   $90,$04,$00,$08,$1C,$09,$02,$B0 ; cmd: NOP $00
        .byte   $9A,$80,$9A,$9A,$80,$9A,$80,$9A ; note data w/ rests
        .byte   $80,$BA,$12,$00,$A2,$B6,$90,$8F ; cmd: loop end G0 fl=$00 → $A2B6
        .byte   $01,$8D,$0E,$01,$A2,$99,$8D,$8C ; cmd: toggle bit 6
        .byte   $8B,$09,$01,$07,$06,$18,$80,$08 ; cmd: set detune $01
        .byte   $11,$03,$C9,$C8,$C9,$CB,$CD,$CE ; cmd: toggle bit 3
        .byte   $0C,$03,$09,$03,$07,$0A,$18,$00 ; cmd: loop start G2 ct=$03
        .byte   $08,$00,$80,$03,$8E,$80,$8F,$80 ; cmd: set pitch ofs $00
        .byte   $90,$80,$91,$0C,$00,$16,$A1,$F7 ; cmd: loop start G2 ct=$00
        .byte   $17,$06,$D2,$07,$08,$08,$02,$09 ; --- Track $05: Top Man stage ch1 (Pulse 2) ---
        .byte   $03,$04,$00,$89,$88,$86,$02,$AB ; cmd: NOP $00
        .byte   $89,$88,$86,$02,$AB,$8C,$8D,$12 ; cmd: set dotted note
        .byte   $00,$A3,$01,$86,$84,$0E,$01,$A2 ; cmd: loop/jmp G0 ct=$01 → $A2E9
        .byte   $E9,$89,$8A,$04,$00,$04,$00,$09 ; cmd: NOP $00
        .byte   $03,$86,$0E,$1F,$A3,$05,$04,$00 ; cmd: loop/jmp G0 ct=$1F → $A305
        .byte   $84,$0E,$17,$A3,$0E,$84,$85,$80 ; cmd: loop/jmp G0 ct=$17 → $A30E
        .byte   $86,$80,$89,$88,$86,$04,$00,$86 ; cmd: NOP $00
        .byte   $0E,$0F,$A3,$1D,$04,$00,$04,$00 ; cmd: loop/jmp G0 ct=$0F → $A31D
        .byte   $86,$0E,$07,$A3,$26,$04,$00,$8B ; cmd: loop/jmp G0 ct=$07 → $A326
        .byte   $0E,$07,$A3,$2D,$04,$00,$86,$86 ; cmd: loop/jmp G0 ct=$07 → $A32D
        .byte   $86,$86,$86,$86,$86,$86,$13,$00 ; cmd: loop end G1 fl=$00 → $A34A
        .byte   $A3,$4A,$0E,$01,$A3,$34,$0F,$03 ; cmd: loop/jmp G0 ct=$01 → $A334
        .byte   $A3,$24,$04,$00,$86,$0E,$05,$A3 ; cmd: NOP $00
        .byte   $4A,$85,$01,$84,$01,$E4,$C0,$80 ; cmd: toggle bit 6
        .byte   $09,$02,$90,$8F,$01,$8D,$01,$CD ; cmd: set detune $02
        .byte   $C0,$C0,$80,$8D,$8C,$8B,$04,$00 ; cmd: NOP $00
        .byte   $8B,$0E,$0F,$A3,$66,$04,$00,$90 ; cmd: loop/jmp G0 ct=$0F → $A366
        .byte   $0E,$07,$A3,$6D,$80,$8E,$80,$8F ; cmd: loop/jmp G0 ct=$07 → $A36D
        .byte   $80,$90,$80,$91,$16,$A3,$03,$17 ; cmd: end channel
        .byte   $06,$BE,$07,$0B,$08,$0C,$E0,$E0 ; --- Track $05: Top Man stage ch0 (Triangle) ---
        .byte   $E0,$C0,$68,$68,$68,$68,$68,$68 ; note data w/ rests
        .byte   $68,$68,$04,$00,$04,$00,$85,$80 ; cmd: NOP $00
        .byte   $88,$80,$85,$85,$88,$80,$12,$00 ; cmd: loop end G0 fl=$00 → $A3AE
        .byte   $A3,$AE,$85,$85,$88,$85,$80,$85 ; note data w/ rests
        .byte   $88,$80,$0E,$03,$A3,$92,$88,$88 ; cmd: loop/jmp G0 ct=$03 → $A392
        .byte   $80,$88,$80,$88,$88,$88,$04,$00 ; cmd: NOP $00
        .byte   $85,$80,$88,$80,$85,$85,$88,$80 ; note data w/ rests
        .byte   $12,$00,$A3,$D0,$85,$80,$88,$85 ; cmd: loop end G0 fl=$00 → $A3D0
        .byte   $80,$85,$88,$80,$0E,$08,$A3,$B6 ; cmd: loop/jmp G0 ct=$08 → $A3B6
        .byte   $85,$80,$88,$85,$80,$88,$88,$88 ; note data w/ rests
        .byte   $07,$0C,$C8,$C0,$C0,$80,$88,$88 ; cmd: set octave 4
        .byte   $88,$C8,$07,$0B,$C0,$C0,$68,$68 ; cmd: set octave 3
        .byte   $68,$68,$68,$68,$68,$68,$04,$00 ; cmd: NOP $00
        .byte   $85,$80,$88,$80,$85,$85,$88,$80 ; note data w/ rests
        .byte   $12,$00,$A4,$08,$85,$85,$88,$85 ; cmd: loop end G0 fl=$00 → $A408
        .byte   $85,$85,$88,$80,$0E,$01,$A3,$EE ; cmd: loop/jmp G0 ct=$01 → $A3EE
        .byte   $88,$85,$88,$85,$88,$85,$88,$85 ; note data
; --- Track $06: Snake Man stage ($A414) ---
        .byte   $16,$A3,$94,$17,$00,$A4,$1D,$A5 ; Track $06 header: $00=music (Snake Man stage)
        .byte   $7D,$A6,$7D,$A7,$61,$05,$02,$00 ; ch2 ptr lo → $A57D
        .byte   $06,$73,$07,$0A,$08,$00,$09,$01 ; cmd: set dur mul $73
        .byte   $04,$08,$73,$93,$73,$93,$73,$93 ; cmd: NOP $08
        .byte   $73,$93,$94,$94,$0E,$01,$A4,$28 ; cmd: loop/jmp G0 ct=$01 → $A428
        .byte   $04,$00,$04,$00,$06,$BE,$18,$00 ; cmd: NOP $00
        .byte   $07,$0A,$08,$00,$02,$9E,$7E,$03 ; cmd: set octave 2
        .byte   $68,$60,$A9,$6B,$60,$6D,$60,$6F ; note data w/ rests
        .byte   $60,$02,$90,$70,$80,$01,$CF,$01 ; cmd: set dotted note
        .byte   $8F,$12,$08,$A4,$79,$02,$95,$75 ; cmd: loop end G0 fl=$08 → $A479
        .byte   $74,$60,$B2,$6D,$60,$70,$60,$72 ; note data w/ rests
        .byte   $60,$02,$92,$72,$70,$60,$AD,$6B ; cmd: set dotted note
        .byte   $60,$69,$60,$68,$60,$0E,$01,$A4 ; cmd: loop/jmp G0 ct=$01 → $A43A
        .byte   $3A,$02,$92,$72,$70,$60,$B2,$75 ; cmd: set dotted note
        .byte   $60,$74,$60,$72,$60,$02,$92,$72 ; cmd: set dotted note
        .byte   $70,$60,$02,$92,$72,$70,$60,$72 ; cmd: set dotted note
        .byte   $60,$70,$60,$04,$00,$06,$78,$07 ; cmd: NOP $00
        .byte   $0B,$18,$40,$08,$1D,$9E,$9E,$7E ; cmd: set duty 25%
        .byte   $9C,$9E,$06,$C8,$07,$09,$08,$17 ; cmd: set dur mul $C8
        .byte   $01,$03,$A9,$01,$69,$08,$1D,$12 ; cmd: toggle bit 6
        .byte   $08,$A4,$B9,$66,$64,$0E,$01,$A4 ; cmd: loop/jmp G0 ct=$01 → $A493
        .byte   $93,$07,$0B,$66,$69,$06,$78,$8D ; cmd: set octave 3
        .byte   $6B,$8B,$89,$89,$86,$66,$89,$8B ; note data
        .byte   $8B,$86,$66,$84,$07,$09,$08,$17 ; cmd: set octave 1
        .byte   $02,$01,$A6,$02,$01,$86,$07,$0B ; cmd: set dotted note
        .byte   $08,$1C,$89,$88,$68,$84,$01,$A1 ; cmd: set pitch ofs $1C
        .byte   $01,$61,$84,$86,$89,$88,$68,$84 ; cmd: toggle bit 6
        .byte   $81,$06,$FA,$07,$09,$08,$17,$02 ; cmd: set dur mul $FA
        .byte   $8B,$07,$0B,$08,$1C,$06,$64,$89 ; cmd: set octave 3
        .byte   $88,$89,$88,$66,$84,$86,$08,$0B ; cmd: set pitch ofs $0B
        .byte   $06,$FF,$02,$95,$74,$60,$72,$60 ; cmd: set dur mul $FF
        .byte   $74,$60,$72,$60,$74,$75,$60,$72 ; note data w/ rests
        .byte   $C0,$18,$80,$08,$1C,$06,$C8,$02 ; cmd: set duty 50%
        .byte   $84,$64,$80,$61,$60,$88,$89,$88 ; note data w/ rests
        .byte   $84,$02,$86,$66,$80,$64,$60,$08 ; cmd: set dotted note
        .byte   $17,$07,$0A,$C6,$08,$1C,$07,$0B ; cmd: set octave 2
        .byte   $02,$88,$68,$80,$68,$60,$88,$89 ; cmd: set dotted note
        .byte   $8B,$88,$02,$8E,$6E,$80,$6D,$60 ; cmd: set dotted note
        .byte   $08,$17,$07,$0A,$CD,$07,$0B,$08 ; cmd: set pitch ofs $17
        .byte   $1C,$02,$8B,$6B,$80,$69,$60,$88 ; cmd: set dotted note
        .byte   $8E,$8D,$88,$A9,$02,$8B,$08,$17 ; cmd: set dotted note
        .byte   $07,$08,$02,$01,$AD,$02,$01,$8D ; cmd: set octave 0
        .byte   $07,$0B,$08,$1C,$02,$86,$66,$80 ; cmd: set octave 3
        .byte   $66,$60,$86,$88,$89,$86,$02,$8E ; cmd: set dotted note
        .byte   $6E,$80,$6D,$60,$08,$17,$07,$09 ; cmd: set pitch ofs $17
        .byte   $CD,$16,$A4,$38,$17,$06,$73,$07 ; --- Track $06: Snake Man stage ch2 (Pulse 1) ---
        .byte   $0A,$08,$00,$09,$01,$04,$08,$6C ; cmd: set pitch ofs $00
        .byte   $8C,$6C,$8C,$6C,$8C,$6C,$8C,$8D ; note data
        .byte   $8D,$0E,$01,$A5,$85,$04,$00,$18 ; cmd: loop/jmp G0 ct=$01 → $A585
        .byte   $00,$08,$07,$07,$09,$06,$E6,$02 ; cmd: set pitch ofs $07
        .byte   $60,$04,$00,$02,$9E,$7E,$03,$68 ; cmd: NOP $00
        .byte   $60,$A9,$6B,$60,$6D,$60,$6F,$60 ; note data w/ rests
        .byte   $02,$90,$70,$80,$01,$CF,$01,$8F ; cmd: set dotted note
        .byte   $12,$08,$A5,$D8,$02,$95,$75,$74 ; cmd: loop end G0 fl=$08 → $A5D8
        .byte   $60,$B2,$6D,$60,$70,$60,$72,$60 ; note data w/ rests
        .byte   $02,$92,$72,$70,$60,$AD,$6B,$60 ; cmd: set dotted note
        .byte   $69,$60,$68,$60,$0E,$01,$A5,$A1 ; cmd: loop/jmp G0 ct=$01 → $A5A1
        .byte   $02,$92,$72,$70,$60,$B2,$75,$60 ; cmd: set dotted note
        .byte   $74,$60,$72,$60,$02,$92,$72,$70 ; cmd: set dotted note
        .byte   $60,$02,$92,$72,$70,$60,$72,$60 ; cmd: set dotted note
        .byte   $50,$04,$00,$08,$09,$18,$80,$07 ; cmd: NOP $00
        .byte   $0B,$06,$C8,$02,$9E,$02,$03,$89 ; cmd: set dur mul $C8
        .byte   $8B,$02,$86,$02,$89,$8B,$0E,$03 ; cmd: set dotted note
        .byte   $A5,$F1,$04,$00,$02,$99,$02,$9C ; cmd: NOP $00
        .byte   $9E,$02,$99,$02,$9C,$9E,$0E,$01 ; cmd: set dotted note
        .byte   $A6,$0A,$04,$00,$02,$9E,$02,$03 ; cmd: NOP $00
        .byte   $89,$8B,$02,$86,$02,$89,$8B,$0E ; cmd: set dotted note
        .byte   $01,$A6,$1A,$07,$05,$18,$40,$06 ; cmd: set octave 5
        .byte   $FF,$08,$11,$02,$CB,$08,$05,$AD ; cmd: set pitch ofs $11
        .byte   $08,$11,$01,$C6,$08,$05,$01,$C6 ; cmd: set pitch ofs $11
        .byte   $08,$11,$01,$CB,$08,$05,$01,$AB ; cmd: set pitch ofs $11
        .byte   $A9,$08,$11,$01,$C4,$08,$05,$01 ; cmd: set pitch ofs $11
        .byte   $C4,$06,$DC,$08,$16,$07,$08,$02 ; cmd: set dur mul $DC
        .byte   $A1,$61,$63,$02,$A5,$66,$68,$02 ; cmd: set dotted note
        .byte   $A9,$69,$6B,$CD,$07,$09,$06,$C8 ; cmd: set octave 1
        .byte   $02,$01,$B2,$02,$01,$92,$71,$70 ; cmd: set dotted note
        .byte   $6F,$6E,$6D,$6B,$69,$02,$AE,$8D ; cmd: set dotted note
        .byte   $CD,$16,$A5,$95,$17,$06,$6E,$08 ; --- Track $06: Snake Man stage ch1 (Pulse 2) ---
        .byte   $02,$09,$03,$04,$00,$66,$86,$66 ; cmd: set detune $03
        .byte   $86,$66,$86,$66,$86,$85,$85,$0E ; cmd: loop/jmp G0 ct=$01 → $A683
        .byte   $01,$A6,$83,$04,$00,$04,$00,$06 ; cmd: NOP $00
        .byte   $6E,$86,$66,$84,$64,$81,$86,$66 ; note data
        .byte   $84,$64,$81,$8B,$6B,$89,$69,$86 ; note data
        .byte   $8B,$6B,$89,$69,$86,$82,$62,$86 ; note data
        .byte   $66,$89,$82,$62,$86,$66,$89,$84 ; note data
        .byte   $64,$88,$68,$8B,$84,$64,$88,$68 ; note data
        .byte   $8B,$0E,$01,$A6,$95,$04,$00,$06 ; cmd: loop/jmp G0 ct=$01 → $A695
        .byte   $C8,$02,$86,$66,$80,$66,$60,$86 ; cmd: set dotted note
        .byte   $88,$89,$8B,$0E,$01,$A6,$C5,$04 ; cmd: loop/jmp G0 ct=$01 → $A6C5
        .byte   $00,$02,$82,$62,$80,$62,$60,$82 ; cmd: set dotted note
        .byte   $84,$86,$89,$0E,$01,$A6,$D7,$04 ; cmd: loop/jmp G0 ct=$01 → $A6D7
        .byte   $00,$02,$84,$64,$80,$64,$60,$84 ; cmd: set dotted note
        .byte   $86,$88,$8B,$0E,$01,$A6,$E7,$04 ; cmd: loop/jmp G0 ct=$01 → $A6E7
        .byte   $00,$02,$86,$66,$80,$66,$60,$86 ; cmd: set dotted note
        .byte   $88,$89,$8B,$0E,$01,$A6,$F7,$02 ; cmd: loop/jmp G0 ct=$01 → $A6F7
        .byte   $81,$61,$80,$61,$60,$02,$81,$61 ; cmd: set dotted note
        .byte   $80,$61,$60,$02,$86,$66,$80,$66 ; cmd: set dotted note
        .byte   $60,$86,$89,$88,$86,$02,$84,$64 ; cmd: set dotted note
        .byte   $80,$64,$60,$02,$84,$64,$80,$64 ; cmd: set dotted note
        .byte   $60,$02,$89,$69,$80,$69,$60,$89 ; cmd: set dotted note
        .byte   $8D,$8B,$89,$02,$88,$68,$80,$68 ; cmd: set dotted note
        .byte   $60,$02,$88,$68,$80,$68,$60,$A6 ; cmd: set dotted note
        .byte   $02,$85,$01,$A4,$01,$64,$A3,$02 ; cmd: set dotted note
        .byte   $82,$62,$80,$62,$60,$02,$82,$62 ; cmd: set dotted note
        .byte   $80,$62,$60,$02,$81,$61,$80,$6B ; cmd: set dotted note
        .byte   $60,$8D,$8B,$88,$8B,$16,$A6,$93 ; cmd: end channel
        .byte   $17,$06,$C8,$07,$0A,$08,$0C,$04 ; --- Track $06: Snake Man stage ch0 (Triangle) ---
        .byte   $00,$68,$68,$60,$68,$68,$60,$68 ; note data w/ rests
        .byte   $68,$60,$68,$68,$60,$68,$60,$68 ; note data w/ rests
        .byte   $60,$0E,$01,$A7,$67,$04,$00,$04 ; cmd: loop/jmp G0 ct=$01 → $A767
        .byte   $00,$07,$0B,$63,$6F,$8D,$68,$6F ; cmd: set octave 3
        .byte   $8D,$63,$6F,$8D,$68,$6F,$8D,$0E ; cmd: loop/jmp G0 ct=$07 → $A77F
        .byte   $07,$A7,$7F,$04,$00,$07,$0A,$63 ; cmd: NOP $00
        .byte   $60,$6F,$6F,$68,$6F,$6F,$63,$60 ; note data w/ rests
        .byte   $60,$63,$60,$68,$6F,$60,$6F,$0E ; cmd: loop/jmp G0 ct=$07 → $A793
        .byte   $07,$A7,$93,$04,$00,$63,$6F,$8D ; cmd: NOP $00
        .byte   $68,$6F,$8D,$63,$6F,$8D,$68,$6F ; note data
        .byte   $8D,$0E,$07,$A7,$AB,$16,$A7,$7D ; cmd: loop/jmp G0 ct=$07 → $A7AB
; --- Track $07: Spark Man stage ($A7C1) ---
        .byte   $17,$00,$A7,$CA,$A8,$99,$A9,$39 ; Track $07 header: $00=music (Spark Man stage)
        .byte   $AA,$6A,$05,$02,$00,$06,$C8,$07 ; ch0 (Triangle) ptr hi
        .byte   $0B,$09,$01,$18,$80,$08,$0B,$04 ; cmd: set detune $01
        .byte   $00,$02,$BB,$7B,$7E,$03,$AB,$02 ; cmd: set dotted note
        .byte   $8A,$66,$02,$A8,$66,$65,$66,$60 ; cmd: set dotted note
        .byte   $65,$63,$65,$60,$63,$61,$02,$A8 ; cmd: set dotted note
        .byte   $68,$6A,$AB,$02,$8A,$6B,$02,$AD ; cmd: set dotted note
        .byte   $6B,$6D,$6F,$60,$6D,$60,$6B,$60 ; note data w/ rests
        .byte   $6A,$60,$0E,$01,$A7,$D7,$04,$08 ; cmd: loop/jmp G0 ct=$01 → $A7D7
        .byte   $18,$C0,$08,$0A,$04,$08,$04,$08 ; cmd: set duty 75%
        .byte   $09,$01,$A8,$02,$88,$66,$68,$60 ; cmd: set detune $01
        .byte   $AB,$12,$08,$A8,$23,$63,$66,$0E ; cmd: loop end G0 fl=$08 → $A823
        .byte   $01,$A8,$0E,$68,$6B,$6D,$60,$6D ; note data w/ rests
        .byte   $60,$6D,$6B,$60,$02,$01,$AD,$02 ; cmd: set dotted note
        .byte   $01,$8D,$02,$90,$02,$91,$72,$60 ; cmd: toggle bit 6
        .byte   $02,$93,$02,$94,$75,$60,$0F,$01 ; cmd: set dotted note
        .byte   $A8,$0C,$04,$08,$18,$80,$08,$1D ; cmd: NOP $08
        .byte   $06,$FF,$94,$72,$60,$90,$6F,$60 ; cmd: set dur mul $FF
        .byte   $70,$6F,$60,$01,$AD,$01,$6D,$92 ; cmd: toggle bit 6
        .byte   $70,$60,$8F,$6D,$60,$6D,$6C,$60 ; note data w/ rests
        .byte   $02,$8F,$88,$04,$08,$02,$90,$02 ; cmd: set dotted note
        .byte   $92,$74,$60,$0E,$01,$A8,$63,$13 ; cmd: loop/jmp G0 ct=$01 → $A863
        .byte   $08,$A8,$83,$02,$90,$02,$92,$94 ; cmd: set dotted note
        .byte   $72,$60,$70,$60,$6F,$02,$8C,$0F ; cmd: set dotted note
        .byte   $01,$A8,$42,$18,$C0,$08,$0A,$09 ; cmd: set duty 75%
        .byte   $00,$06,$C8,$77,$60,$77,$60,$77 ; cmd: set dur mul $C8
        .byte   $77,$60,$77,$B7,$B8,$16,$A8,$06 ; cmd: end channel
        .byte   $17,$06,$FF,$09,$01,$07,$0C,$08 ; --- Track $07: Spark Man stage ch2 (Pulse 1) ---
        .byte   $0D,$18,$40,$04,$08,$12,$08,$A8 ; cmd: set duty 25%
        .byte   $B1,$6A,$6B,$6A,$68,$0E,$1E,$A8 ; cmd: loop/jmp G0 ct=$1E → $A8A3
        .byte   $A3,$68,$6A,$6B,$6F,$74,$76,$77 ; note data
        .byte   $6F,$04,$00,$04,$00,$04,$00,$18 ; cmd: NOP $00
        .byte   $00,$09,$02,$08,$0F,$07,$0C,$74 ; cmd: set detune $02
        .byte   $60,$6F,$60,$72,$74,$60,$02,$94 ; cmd: set dotted note
        .byte   $6F,$60,$72,$74,$60,$12,$00,$A8 ; cmd: loop end G0 fl=$00 → $A8DF
        .byte   $DF,$01,$74,$0E,$02,$A8,$BD,$74 ; cmd: toggle bit 6
        .byte   $07,$0A,$08,$0A,$02,$97,$02,$98 ; cmd: set octave 2
        .byte   $79,$60,$02,$9A,$02,$9B,$7C,$60 ; cmd: set dotted note
        .byte   $0F,$01,$A8,$BB,$18,$40,$07,$06 ; cmd: loop/jmp G1 ct=$01 → $A8BB
        .byte   $09,$01,$04,$48,$08,$10,$CD,$08 ; cmd: set detune $01
        .byte   $05,$01,$CD,$08,$10,$01,$D0,$08 ; cmd: toggle bit 6
        .byte   $05,$01,$B0,$AF,$08,$10,$01,$C8 ; cmd: toggle bit 6
        .byte   $08,$05,$01,$C8,$12,$08,$A9,$25 ; cmd: set pitch ofs $05
        .byte   $08,$10,$01,$D2,$08,$05,$01,$B2 ; cmd: set pitch ofs $10
        .byte   $B0,$0E,$01,$A8,$FA,$07,$09,$08 ; cmd: loop/jmp G0 ct=$01 → $A8FA
        .byte   $0A,$09,$01,$66,$60,$66,$60,$66 ; cmd: set detune $01
        .byte   $66,$60,$66,$A6,$A7,$16,$A8,$B9 ; cmd: end channel
        .byte   $17,$06,$C8,$08,$02,$09,$03,$04 ; --- Track $07: Spark Man stage ch1 (Pulse 2) ---
        .byte   $00,$68,$60,$68,$68,$0E,$03,$A9 ; cmd: loop/jmp G0 ct=$03 → $A93F
        .byte   $3F,$04,$00,$61,$60,$61,$61,$0E ; cmd: NOP $00
        .byte   $03,$A9,$49,$04,$00,$64,$60,$64 ; cmd: NOP $00
        .byte   $64,$0E,$03,$A9,$53,$04,$00,$66 ; cmd: loop/jmp G0 ct=$03 → $A953
        .byte   $60,$66,$66,$0E,$03,$A9,$5D,$09 ; cmd: loop/jmp G0 ct=$03 → $A95D
        .byte   $01,$08,$0E,$60,$03,$74,$71,$6D ; cmd: set pitch ofs $0E
        .byte   $06,$E6,$8C,$06,$C8,$08,$02,$09 ; cmd: set dur mul $E6
        .byte   $03,$03,$68,$68,$68,$60,$68,$68 ; cmd: toggle bit 3
        .byte   $68,$60,$68,$68,$61,$60,$09,$01 ; cmd: set detune $01
        .byte   $08,$0E,$06,$E6,$03,$94,$60,$8E ; cmd: set pitch ofs $0E
        .byte   $60,$06,$C8,$74,$74,$8F,$74,$74 ; cmd: set dur mul $C8
        .byte   $8F,$04,$00,$08,$02,$09,$03,$64 ; cmd: NOP $00
        .byte   $60,$64,$64,$0E,$03,$A9,$99,$08 ; cmd: loop/jmp G0 ct=$03 → $A999
        .byte   $0E,$09,$01,$03,$74,$74,$06,$E6 ; cmd: set detune $01
        .byte   $8E,$06,$C8,$74,$74,$06,$E6,$8E ; cmd: set dur mul $C8
        .byte   $06,$C8,$6D,$71,$74,$6D,$71,$74 ; cmd: set dur mul $C8
        .byte   $71,$6D,$04,$00,$04,$00,$04,$00 ; cmd: NOP $00
        .byte   $08,$02,$09,$03,$06,$C8,$68,$60 ; cmd: set pitch ofs $02
        .byte   $68,$68,$06,$E6,$08,$0E,$09,$01 ; cmd: set dur mul $E6
        .byte   $03,$94,$08,$02,$09,$03,$03,$68 ; cmd: toggle bit 3
        .byte   $68,$0E,$03,$A9,$C6,$04,$00,$69 ; cmd: loop/jmp G0 ct=$03 → $A9C6
        .byte   $60,$69,$69,$08,$0E,$09,$01,$06 ; cmd: set pitch ofs $0E
        .byte   $E6,$03,$92,$08,$02,$09,$03,$03 ; cmd: toggle bit 3
        .byte   $69,$69,$0E,$03,$A9,$E5,$0F,$01 ; cmd: loop/jmp G0 ct=$03 → $A9E5
        .byte   $A9,$C4,$04,$00,$04,$00,$61,$60 ; cmd: NOP $00
        .byte   $61,$61,$0E,$01,$AA,$04,$04,$00 ; cmd: loop/jmp G0 ct=$01 → $AA04
        .byte   $66,$60,$66,$66,$0E,$01,$AA,$0E ; cmd: loop/jmp G0 ct=$01 → $AA0E
        .byte   $04,$00,$63,$60,$63,$63,$0E,$01 ; cmd: NOP $00
        .byte   $AA,$18,$04,$00,$68,$60,$68,$68 ; cmd: NOP $00
        .byte   $0E,$01,$AA,$22,$04,$00,$6D,$60 ; cmd: loop/jmp G0 ct=$01 → $AA22
        .byte   $6D,$6D,$0E,$01,$AA,$2C,$04,$00 ; cmd: loop/jmp G0 ct=$01 → $AA2C
        .byte   $6B,$60,$6B,$6B,$0E,$01,$AA,$36 ; cmd: loop/jmp G0 ct=$01 → $AA36
        .byte   $04,$00,$13,$00,$AA,$5C,$69,$60 ; cmd: NOP $00
        .byte   $69,$69,$0E,$01,$AA,$40,$04,$00 ; cmd: loop/jmp G0 ct=$01 → $AA40
        .byte   $68,$60,$68,$68,$0E,$01,$AA,$4E ; cmd: loop/jmp G0 ct=$01 → $AA4E
        .byte   $0F,$01,$AA,$02,$6B,$60,$6B,$60 ; cmd: loop/jmp G1 ct=$01 → $AA02
        .byte   $6B,$6B,$60,$6B,$AB,$AC,$16,$A9 ; cmd: end channel
        .byte   $C2,$17,$06,$C8,$07,$0C,$08,$0C ; --- Track $07: Spark Man stage ch0 (Triangle) ---
        .byte   $04,$00,$63,$60,$6F,$6F,$0E,$1B ; cmd: NOP $00
        .byte   $AA,$70,$63,$6F,$6D,$6F,$68,$6F ; note data
        .byte   $6D,$6F,$6C,$68,$63,$6C,$68,$63 ; note data
        .byte   $6C,$68,$04,$00,$04,$00,$63,$60 ; cmd: NOP $00
        .byte   $6F,$6F,$68,$60,$6F,$6F,$0E,$0F ; cmd: loop/jmp G0 ct=$0F → $AA8C
        .byte   $AA,$8C,$04,$00,$64,$6F,$8D,$68 ; cmd: NOP $00
        .byte   $6F,$6D,$64,$6F,$64,$8D,$68,$6F ; note data
        .byte   $6D,$60,$0E,$06,$AA,$9A,$68,$60 ; cmd: loop/jmp G0 ct=$06 → $AA9A
        .byte   $68,$60,$68,$68,$60,$68,$6C,$6A ; note data w/ rests
        .byte   $68,$63,$6C,$6A,$68,$63,$16,$AA ; cmd: end channel
; --- Track $08: Shadow Man stage ($AAC2) ---
        .byte   $8A,$17,$00,$AA,$CB,$AC,$02,$AD ; Track $08 header: $00=music (Shadow Man stage)
        .byte   $1C,$AD,$A6,$0A,$03,$04,$00,$18 ; ch1 ptr lo → $AD1C
        .byte   $40,$08,$0B,$05,$02,$2E,$06,$FF ; cmd: set pitch ofs $0B
        .byte   $07,$0B,$7F,$7C,$7F,$03,$69,$6C ; cmd: set octave 3
        .byte   $6A,$6B,$6C,$60,$70,$60,$73,$60 ; note data w/ rests
        .byte   $75,$60,$76,$97,$80,$97,$80,$97 ; note data w/ rests
        .byte   $80,$97,$80,$04,$00,$07,$08,$B0 ; cmd: NOP $00
        .byte   $B3,$D5,$80,$96,$95,$01,$93,$01 ; cmd: toggle bit 6
        .byte   $D3,$0E,$01,$AA,$F3,$04,$00,$04 ; cmd: loop/jmp G0 ct=$01 → $AAF3
        .byte   $00,$18,$40,$08,$07,$09,$02,$07 ; cmd: set duty 25%
        .byte   $0B,$06,$B4,$A0,$03,$A9,$89,$02 ; cmd: set dur mul $B4
        .byte   $A7,$13,$08,$AB,$26,$89,$AB,$A9 ; cmd: loop end G1 fl=$08 → $AB26
        .byte   $02,$A7,$0F,$01,$AB,$07,$8D,$A0 ; cmd: set dotted note
        .byte   $08,$07,$01,$A7,$02,$01,$A7,$0E ; cmd: set pitch ofs $07
        .byte   $01,$AB,$05,$04,$08,$08,$0B,$09 ; cmd: NOP $08
        .byte   $00,$07,$0C,$69,$60,$69,$60,$87 ; cmd: set octave 4
        .byte   $69,$60,$80,$89,$87,$89,$4A,$01 ; cmd: toggle bit 6
        .byte   $4B,$01,$6B,$89,$87,$01,$A4,$08 ; cmd: toggle bit 6
        .byte   $08,$01,$A4,$80,$08,$0B,$69,$60 ; cmd: toggle bit 6
        .byte   $69,$60,$87,$69,$60,$80,$4F,$01 ; cmd: toggle bit 6
        .byte   $50,$01,$70,$8F,$01,$8E,$8E,$08 ; cmd: toggle bit 6
        .byte   $08,$AE,$01,$CE,$80,$08,$0B,$73 ; cmd: toggle bit 6
        .byte   $60,$73,$60,$90,$73,$60,$80,$93 ; note data w/ rests
        .byte   $90,$93,$54,$01,$55,$02,$01,$95 ; cmd: toggle bit 6
        .byte   $B7,$95,$90,$A0,$6E,$60,$6E,$60 ; note data w/ rests
        .byte   $8B,$6E,$60,$80,$52,$01,$53,$01 ; cmd: toggle bit 6
        .byte   $73,$92,$01,$90,$90,$08,$08,$B0 ; cmd: toggle bit 6
        .byte   $01,$D0,$80,$0E,$01,$AB,$33,$08 ; cmd: toggle bit 6
        .byte   $0B,$64,$62,$64,$60,$67,$60,$69 ; note data w/ rests
        .byte   $67,$69,$60,$6B,$60,$6E,$6B,$6E ; note data w/ rests
        .byte   $60,$70,$60,$73,$60,$75,$60,$01 ; cmd: toggle bit 6
        .byte   $97,$08,$08,$01,$D7,$04,$08,$06 ; cmd: set pitch ofs $08
        .byte   $C8,$18,$C0,$08,$0B,$07,$0C,$6E ; cmd: set duty 75%
        .byte   $69,$6E,$60,$70,$60,$73,$60,$51 ; note data w/ rests
        .byte   $01,$52,$72,$01,$72,$6E,$60,$80 ; cmd: toggle bit 6
        .byte   $01,$77,$01,$97,$75,$60,$73,$60 ; cmd: toggle bit 6
        .byte   $72,$60,$4F,$01,$50,$01,$90,$72 ; cmd: toggle bit 6
        .byte   $60,$80,$6E,$B5,$B3,$B2,$02,$90 ; cmd: set dotted note
        .byte   $70,$60,$02,$8E,$6E,$60,$6A,$60 ; cmd: set dotted note
        .byte   $AE,$B0,$0E,$01,$AB,$BD,$16,$AA ; cmd: loop/jmp G0 ct=$01 → $ABBD
        .byte   $CD,$17,$04,$00,$18,$00,$06,$FF ; --- Track $08: Shadow Man stage ch2 (Pulse 1) ---
        .byte   $07,$0B,$08,$0B,$09,$00,$73,$70 ; cmd: set octave 3
        .byte   $73,$75,$78,$76,$77,$78,$60,$7C ; note data w/ rests
        .byte   $60,$7F,$60,$03,$69,$60,$6A,$90 ; cmd: toggle bit 3
        .byte   $80,$90,$80,$90,$80,$90,$80,$04 ; cmd: NOP $00
        .byte   $00,$E0,$E0,$E0,$E0,$04,$00,$04 ; cmd: NOP $00
        .byte   $00,$18,$80,$09,$01,$08,$1C,$06 ; cmd: set duty 50%
        .byte   $B4,$A0,$03,$90,$80,$90,$90,$A0 ; cmd: toggle bit 3
        .byte   $12,$08,$AC,$4F,$90,$90,$80,$90 ; cmd: loop end G0 fl=$08 → $AC4F
        .byte   $80,$02,$B0,$0E,$01,$AC,$2F,$AE ; cmd: set dotted note
        .byte   $80,$01,$8E,$01,$CE,$0F,$01,$AC ; cmd: toggle bit 6
        .byte   $2D,$04,$48,$18,$C0,$09,$01,$07 ; cmd: NOP $48
        .byte   $04,$08,$11,$CD,$08,$05,$01,$CD ; cmd: set pitch ofs $11
        .byte   $08,$11,$01,$C7,$08,$05,$01,$C7 ; cmd: set pitch ofs $11
        .byte   $12,$08,$AC,$89,$08,$11,$01,$CD ; cmd: loop end G0 fl=$08 → $AC89
        .byte   $08,$05,$01,$AD,$A7,$08,$11,$01 ; cmd: set pitch ofs $05
        .byte   $C4,$08,$05,$01,$C4,$0E,$01,$AC ; cmd: set pitch ofs $05
        .byte   $59,$02,$C9,$A7,$E4,$04,$00,$04 ; cmd: set dotted note
        .byte   $00,$08,$04,$09,$02,$07,$09,$7C ; cmd: set pitch ofs $04
        .byte   $7A,$7C,$7F,$03,$6B,$67,$64,$62 ; cmd: toggle bit 3
        .byte   $0E,$02,$AC,$8F,$6E,$6D,$6C,$6B ; cmd: loop/jmp G0 ct=$02 → $AC8F
        .byte   $6A,$69,$68,$67,$0F,$03,$AC,$8D ; cmd: loop/jmp G1 ct=$03 → $AC8D
        .byte   $08,$06,$07,$08,$09,$01,$70,$69 ; cmd: set pitch ofs $06
        .byte   $6E,$70,$69,$6E,$70,$69,$6E,$70 ; note data
        .byte   $69,$6E,$70,$69,$6E,$70,$69,$6E ; note data
        .byte   $70,$69,$6E,$70,$69,$6E,$70,$69 ; note data
        .byte   $6E,$70,$75,$73,$72,$70,$04,$00 ; cmd: NOP $00
        .byte   $06,$C8,$18,$40,$08,$0B,$07,$0A ; cmd: set dur mul $C8
        .byte   $7E,$7A,$7E,$60,$03,$69,$60,$69 ; cmd: toggle bit 3
        .byte   $60,$48,$01,$49,$01,$89,$66,$60 ; cmd: toggle bit 6
        .byte   $80,$01,$67,$01,$87,$66,$60,$64 ; cmd: toggle bit 6
        .byte   $60,$62,$60,$03,$58,$01,$59,$01 ; cmd: toggle bit 3
        .byte   $99,$7A,$60,$80,$77,$BE,$BC,$BA ; note data w/ rests
        .byte   $02,$99,$73,$60,$02,$96,$76,$60 ; cmd: set dotted note
        .byte   $76,$60,$B8,$B8,$0E,$01,$AC,$D6 ; cmd: loop/jmp G0 ct=$01 → $ACD6
        .byte   $16,$AC,$02,$17,$04,$00,$06,$F0 ; --- Track $08: Shadow Man stage ch1 (Pulse 2) ---
        .byte   $08,$02,$09,$01,$0D,$50,$E0,$95 ; cmd: set pitch ofs $02
        .byte   $80,$95,$80,$95,$80,$95,$80,$04 ; cmd: NOP $00
        .byte   $00,$B0,$B3,$D5,$80,$96,$95,$01 ; cmd: toggle bit 6
        .byte   $93,$01,$D3,$0E,$05,$AD,$2F,$04 ; cmd: toggle bit 6
        .byte   $00,$04,$00,$04,$00,$0D,$00,$90 ; cmd: NOP $00
        .byte   $90,$95,$90,$80,$90,$95,$80,$0E ; cmd: loop/jmp G0 ct=$02 → $AD43
        .byte   $02,$AD,$43,$13,$00,$AD,$63,$93 ; cmd: loop end G1 fl=$00 → $AD63
        .byte   $93,$9A,$93,$9D,$9C,$9A,$93,$0F ; cmd: loop/jmp G1 ct=$01 → $AD41
        .byte   $01,$AD,$41,$90,$90,$93,$90,$97 ; note data
        .byte   $95,$93,$90,$10,$01,$AD,$3F,$F5 ; cmd: loop/jmp G2 ct=$01 → $AD3F
        .byte   $F3,$04,$00,$04,$00,$7A,$60,$7A ; cmd: NOP $00
        .byte   $60,$75,$77,$60,$02,$98,$78,$60 ; cmd: set dotted note
        .byte   $78,$60,$80,$0E,$01,$AD,$73,$73 ; cmd: loop/jmp G0 ct=$01 → $AD73
        .byte   $60,$73,$60,$70,$73,$60,$02,$95 ; cmd: set dotted note
        .byte   $75,$60,$75,$60,$80,$76,$60,$76 ; note data w/ rests
        .byte   $60,$73,$76,$80,$B8,$B8,$0F,$01 ; cmd: loop/jmp G1 ct=$01 → $AD71
        .byte   $AD,$71,$16,$AD,$1C,$17,$04,$00 ; --- Track $08: Shadow Man stage ch0 (Triangle) ---
        .byte   $06,$FF,$07,$0A,$08,$0C,$E0,$E0 ; cmd: set dur mul $FF
        .byte   $04,$00,$63,$60,$63,$60,$68,$63 ; cmd: NOP $00
        .byte   $60,$63,$80,$63,$60,$68,$60,$63 ; note data w/ rests
        .byte   $60,$0E,$1B,$AD,$B0,$04,$00,$A3 ; cmd: loop/jmp G0 ct=$1B → $ADB0
        .byte   $0E,$07,$AD,$C5,$04,$00,$63,$60 ; cmd: loop/jmp G0 ct=$07 → $ADC5
        .byte   $63,$60,$68,$63,$60,$63,$80,$63 ; note data w/ rests
        .byte   $60,$68,$60,$63,$60,$0E,$07,$AD ; cmd: loop/jmp G0 ct=$07 → $ADCC
; --- Track $09: Wily Fortress 1-2 ($ADE5) ---
        .byte   $CC,$16,$AD,$A6,$17,$00,$AD,$EE ; Track $09 header: $00=music (Wily Fortress 1-2)
        .byte   $AE,$DF,$AF,$A1,$B0,$F1,$0A,$0B ; ch2 (Pulse 1) ptr hi
        .byte   $05,$01,$EB,$06,$FA,$07,$0A,$08 ; cmd: set tempo $01,$EB
        .byte   $1D,$18,$80,$76,$74,$76,$78,$76 ; cmd: set duty 50%
        .byte   $78,$79,$78,$79,$7B,$79,$7B,$7D ; note data
        .byte   $7B,$7D,$03,$68,$04,$08,$04,$08 ; cmd: toggle bit 3
        .byte   $18,$40,$08,$1D,$02,$AC,$02,$AD ; cmd: set duty 25%
        .byte   $01,$AC,$01,$8C,$02,$AD,$6C,$02 ; cmd: toggle bit 6
        .byte   $80,$68,$02,$80,$02,$AC,$02,$AD ; cmd: set dotted note
        .byte   $01,$AC,$01,$8C,$02,$AD,$8C,$80 ; cmd: toggle bit 6
        .byte   $88,$80,$02,$01,$C7,$08,$14,$01 ; cmd: set dotted note
        .byte   $A7,$E7,$0E,$01,$AE,$0E,$04,$08 ; cmd: loop/jmp G0 ct=$01 → $AE0E
        .byte   $07,$0D,$08,$12,$06,$C8,$68,$02 ; cmd: set octave 5
        .byte   $80,$67,$02,$80,$65,$02,$80,$63 ; cmd: set dotted note
        .byte   $80,$01,$65,$01,$85,$03,$76,$02 ; cmd: toggle bit 6
        .byte   $80,$76,$60,$96,$99,$BB,$12,$00 ; cmd: loop end G0 fl=$00 → $AE7A
        .byte   $AE,$7A,$7D,$02,$80,$7B,$02,$80 ; cmd: set dotted note
        .byte   $79,$80,$78,$80,$06,$FA,$01,$94 ; cmd: set dur mul $FA
        .byte   $B4,$08,$14,$02,$01,$D4,$0E,$01 ; cmd: set pitch ofs $14
        .byte   $AE,$3E,$7D,$02,$80,$7B,$02,$80 ; cmd: set dotted note
        .byte   $03,$68,$80,$67,$80,$06,$FA,$01 ; cmd: toggle bit 3
        .byte   $83,$A3,$08,$14,$02,$01,$C3,$04 ; cmd: set pitch ofs $14
        .byte   $00,$08,$1D,$07,$0C,$80,$03,$68 ; cmd: set pitch ofs $1D
        .byte   $80,$68,$60,$68,$88,$87,$12,$08 ; cmd: loop end G0 fl=$08 → $AEA7
        .byte   $AE,$A7,$AA,$0E,$02,$AE,$8F,$6A ; cmd: loop/jmp G0 ct=$02 → $AE8F
        .byte   $60,$01,$81,$A1,$08,$14,$02,$01 ; cmd: toggle bit 6
        .byte   $C1,$04,$00,$08,$1D,$80,$03,$6B ; cmd: NOP $00
        .byte   $80,$6B,$60,$6B,$8B,$8A,$12,$08 ; cmd: loop end G0 fl=$08 → $AEC7
        .byte   $AE,$C7,$AD,$0E,$02,$AE,$B1,$6D ; cmd: loop/jmp G0 ct=$02 → $AEB1
        .byte   $60,$6B,$6A,$6B,$60,$6A,$68,$6A ; note data w/ rests
        .byte   $60,$68,$66,$68,$60,$66,$65,$66 ; note data w/ rests
        .byte   $60,$68,$6A,$16,$AE,$0C,$17,$06 ; --- Track $09: Wily Fortress 1-2 ch2 (Pulse 1) ---
        .byte   $FA,$07,$0C,$08,$1D,$18,$C0,$72 ; cmd: set octave 4
        .byte   $71,$72,$74,$72,$74,$76,$74,$76 ; note data
        .byte   $78,$76,$78,$79,$78,$79,$7D,$04 ; cmd: NOP $08
        .byte   $08,$09,$00,$04,$08,$08,$1A,$02 ; cmd: set detune $00
        .byte   $A8,$02,$AA,$01,$A8,$01,$88,$02 ; cmd: set dotted note
        .byte   $AA,$88,$80,$85,$80,$02,$A8,$02 ; cmd: set dotted note
        .byte   $AA,$01,$A8,$01,$88,$02,$AA,$88 ; cmd: toggle bit 6
        .byte   $80,$85,$80,$08,$1D,$80,$6A,$60 ; cmd: set pitch ofs $1D
        .byte   $6D,$60,$6F,$60,$01,$6F,$0D,$1E ; cmd: toggle bit 6
        .byte   $01,$91,$0D,$00,$6F,$80,$6A,$60 ; cmd: toggle bit 6
        .byte   $01,$8D,$0D,$1E,$8F,$01,$8D,$0D ; cmd: toggle bit 6
        .byte   $00,$8C,$02,$8A,$68,$80,$8A,$0E ; cmd: set dotted note
        .byte   $01,$AE,$FB,$04,$00,$07,$08,$08 ; cmd: NOP $00
        .byte   $12,$06,$64,$80,$03,$6A,$60,$68 ; cmd: set dur mul $64
        .byte   $02,$80,$6D,$68,$60,$6A,$A0,$0E ; cmd: set dotted note
        .byte   $07,$AF,$43,$04,$08,$07,$0C,$08 ; cmd: NOP $08
        .byte   $15,$06,$FA,$6A,$03,$76,$76,$60 ; cmd: set dur mul $FA
        .byte   $76,$80,$76,$80,$76,$80,$76,$80 ; note data w/ rests
        .byte   $0E,$03,$AF,$5B,$04,$08,$6D,$61 ; cmd: loop/jmp G0 ct=$03 → $AF5B
        .byte   $61,$60,$61,$80,$61,$12,$08,$AF ; cmd: loop end G0 fl=$08 → $AF8A
        .byte   $8A,$80,$61,$80,$61,$80,$0E,$03 ; cmd: loop/jmp G0 ct=$03 → $AF74
        .byte   $AF,$74,$08,$1D,$09,$01,$00,$03 ; cmd: set pitch ofs $1D
        .byte   $6F,$71,$73,$74,$76,$78,$7A,$7B ; note data
        .byte   $7D,$7F,$03,$68,$6A,$16,$AE,$F7 ; cmd: toggle bit 3
        .byte   $17,$06,$C8,$08,$00,$09,$02,$02 ; --- Track $09: Wily Fortress 1-2 ch1 (Pulse 2) ---
        .byte   $8A,$02,$88,$02,$86,$02,$83,$65 ; cmd: set dotted note
        .byte   $60,$68,$60,$04,$00,$09,$02,$6A ; cmd: NOP $00
        .byte   $02,$80,$02,$8A,$6A,$8A,$6A,$02 ; cmd: set dotted note
        .byte   $80,$6A,$60,$80,$6A,$60,$02,$8A ; cmd: set dotted note
        .byte   $6A,$8D,$8A,$8D,$8F,$6A,$02,$80 ; cmd: set dotted note
        .byte   $02,$8A,$6A,$8A,$6A,$02,$80,$6A ; cmd: set dotted note
        .byte   $60,$80,$6A,$60,$02,$8A,$6A,$8A ; cmd: set dotted note
        .byte   $84,$83,$81,$6A,$02,$80,$02,$8A ; cmd: set dotted note
        .byte   $6A,$8A,$6A,$02,$80,$6A,$60,$80 ; cmd: set dotted note
        .byte   $6A,$60,$02,$8A,$6A,$8D,$8A,$8D ; cmd: set dotted note
        .byte   $8F,$6A,$02,$80,$02,$8A,$6A,$8A ; cmd: set dotted note
        .byte   $6A,$02,$80,$6A,$60,$80,$6A,$60 ; cmd: set dotted note
        .byte   $02,$8A,$6A,$8A,$84,$83,$81,$6A ; cmd: set dotted note
        .byte   $02,$80,$02,$8A,$6A,$8A,$6A,$02 ; cmd: set dotted note
        .byte   $80,$6A,$60,$80,$6A,$60,$02,$8A ; cmd: set dotted note
        .byte   $6A,$8D,$8A,$8D,$8F,$6A,$02,$80 ; cmd: set dotted note
        .byte   $02,$8A,$6A,$8A,$6A,$02,$80,$6A ; cmd: set dotted note
        .byte   $60,$80,$6A,$60,$02,$8A,$6A,$8A ; cmd: set dotted note
        .byte   $84,$83,$81,$6A,$02,$80,$02,$8A ; cmd: set dotted note
        .byte   $6A,$8A,$6A,$02,$80,$6A,$60,$80 ; cmd: set dotted note
        .byte   $6A,$60,$02,$8A,$6A,$8D,$8A,$8D ; cmd: set dotted note
        .byte   $8F,$6A,$02,$80,$02,$8A,$6A,$8A ; cmd: set dotted note
        .byte   $6A,$02,$80,$6A,$60,$80,$6A,$60 ; cmd: set dotted note
        .byte   $02,$8A,$6A,$8A,$84,$83,$81,$6A ; cmd: set dotted note
        .byte   $02,$80,$02,$8A,$6A,$8A,$6A,$02 ; cmd: set dotted note
        .byte   $80,$6A,$60,$80,$6A,$60,$02,$8A ; cmd: set dotted note
        .byte   $6A,$8D,$8A,$8D,$8F,$6A,$02,$80 ; cmd: set dotted note
        .byte   $02,$8A,$6A,$8A,$6A,$02,$80,$6A ; cmd: set dotted note
        .byte   $60,$80,$6A,$60,$02,$8A,$6A,$8A ; cmd: set dotted note
        .byte   $84,$83,$81,$6F,$02,$80,$02,$8F ; cmd: set dotted note
        .byte   $6A,$8D,$6F,$02,$80,$6F,$60,$80 ; cmd: set dotted note
        .byte   $6F,$60,$02,$8F,$6F,$8F,$88,$8A ; cmd: set dotted note
        .byte   $8D,$6F,$02,$80,$02,$8F,$6A,$8D ; cmd: set dotted note
        .byte   $6F,$02,$80,$6F,$60,$80,$6F,$60 ; cmd: set dotted note
        .byte   $02,$8F,$6F,$8F,$89,$88,$86,$66 ; cmd: set dotted note
        .byte   $02,$80,$02,$86,$61,$84,$66,$02 ; cmd: set dotted note
        .byte   $80,$66,$60,$80,$66,$60,$02,$86 ; cmd: set dotted note
        .byte   $66,$86,$09,$01,$8B,$8D,$90,$72 ; cmd: set detune $01
        .byte   $02,$80,$02,$92,$6D,$90,$72,$02 ; cmd: set dotted note
        .byte   $80,$72,$60,$80,$72,$60,$02,$92 ; cmd: set dotted note
        .byte   $72,$92,$8C,$8B,$89,$16,$AF,$B3 ; cmd: end channel
        .byte   $17,$06,$FF,$07,$0C,$08,$0C,$02 ; --- Track $09: Wily Fortress 1-2 ch0 (Triangle) ---
        .byte   $8C,$02,$8C,$02,$8C,$02,$8C,$68 ; cmd: set dotted note
        .byte   $68,$68,$68,$04,$00,$06,$C8,$07 ; cmd: NOP $00
        .byte   $0A,$04,$00,$65,$6D,$6D,$60,$68 ; cmd: NOP $00
        .byte   $6D,$6D,$65,$6D,$6D,$65,$6D,$68 ; note data
        .byte   $6C,$6D,$6F,$0E,$1B,$B1,$09,$16 ; cmd: loop/jmp G0 ct=$1B → $B109
; --- Track $0A: Wily Fortress 3-4 ($B123) ---
        .byte   $B1,$03,$17,$00,$B1,$2C,$B1,$D6 ; Track $0A header: $00=music (Wily Fortress 3-4)
        .byte   $B2,$5A,$B2,$A3,$0A,$02,$05,$02 ; ch1 (Pulse 2) ptr hi
        .byte   $00,$06,$C8,$07,$0C,$08,$1D,$09 ; cmd: set dur mul $C8
        .byte   $01,$18,$40,$04,$00,$7D,$7B,$03 ; cmd: set duty 25%
        .byte   $68,$66,$0E,$02,$B1,$3B,$65,$63 ; cmd: loop/jmp G0 ct=$02 → $B13B
        .byte   $61,$03,$78,$04,$00,$04,$00,$08 ; cmd: toggle bit 3
        .byte   $1D,$9B,$80,$9B,$80,$80,$7B,$02 ; cmd: set dotted note
        .byte   $80,$79,$02,$80,$96,$98,$96,$98 ; cmd: set dotted note
        .byte   $99,$98,$96,$0E,$01,$B1,$4D,$04 ; cmd: loop/jmp G0 ct=$01 → $B14D
        .byte   $00,$9D,$80,$9D,$80,$80,$7D,$02 ; cmd: set dotted note
        .byte   $80,$79,$02,$80,$A0,$76,$78,$99 ; cmd: set dotted note
        .byte   $98,$99,$9B,$9D,$80,$9D,$80,$80 ; note data w/ rests
        .byte   $7D,$02,$80,$DD,$7E,$7D,$DB,$04 ; cmd: set dotted note
        .byte   $00,$08,$1D,$D6,$D8,$D9,$DB,$01 ; cmd: set pitch ofs $1D
        .byte   $DD,$08,$14,$02,$01,$BD,$08,$1D ; cmd: set pitch ofs $14
        .byte   $7E,$03,$68,$01,$C5,$08,$14,$02 ; cmd: toggle bit 3
        .byte   $01,$A5,$08,$1D,$63,$61,$01,$C3 ; cmd: toggle bit 6
        .byte   $08,$14,$02,$01,$A3,$08,$1D,$03 ; cmd: set pitch ofs $14
        .byte   $76,$60,$01,$D6,$08,$14,$02,$01 ; cmd: toggle bit 6
        .byte   $B6,$08,$1D,$76,$79,$01,$D9,$08 ; cmd: set pitch ofs $1D
        .byte   $14,$02,$01,$B9,$08,$1D,$78,$60 ; cmd: set dotted note
        .byte   $01,$D8,$08,$14,$01,$D8,$0E,$01 ; cmd: toggle bit 6
        .byte   $B1,$87,$16,$B1,$4B,$17,$06,$C8 ; --- Track $0A: Wily Fortress 3-4 ch2 (Pulse 1) ---
        .byte   $07,$0C,$08,$1D,$18,$80,$09,$01 ; cmd: set octave 4
        .byte   $04,$00,$79,$78,$7D,$7B,$0E,$02 ; cmd: NOP $00
        .byte   $B1,$E0,$7D,$7B,$79,$78,$04,$08 ; cmd: NOP $08
        .byte   $04,$08,$18,$40,$09,$00,$68,$02 ; cmd: NOP $08
        .byte   $80,$68,$02,$80,$80,$68,$02,$80 ; cmd: set dotted note
        .byte   $68,$02,$80,$04,$00,$9E,$0E,$06 ; cmd: set dotted note
        .byte   $B2,$03,$0F,$01,$B1,$F0,$04,$08 ; cmd: loop/jmp G1 ct=$01 → $B1F0
        .byte   $6A,$02,$80,$6A,$02,$80,$80,$6A ; cmd: set dotted note
        .byte   $02,$80,$6A,$60,$13,$08,$B2,$2B ; cmd: set dotted note
        .byte   $04,$00,$9E,$0E,$07,$B2,$20,$0F ; cmd: NOP $00
        .byte   $01,$B2,$0E,$CA,$C9,$04,$00,$04 ; cmd: NOP $00
        .byte   $00,$9D,$0E,$0F,$B2,$2F,$04,$00 ; cmd: loop/jmp G0 ct=$0F → $B22F
        .byte   $99,$0E,$0F,$B2,$36,$04,$00,$9B ; cmd: loop/jmp G0 ct=$0F → $B236
        .byte   $0E,$0F,$B2,$3D,$04,$00,$96,$0E ; cmd: loop/jmp G0 ct=$0F → $B23D
        .byte   $07,$B2,$44,$04,$00,$95,$0E,$07 ; cmd: NOP $00
        .byte   $B2,$4B,$0F,$01,$B2,$2D,$16,$B1 ; cmd: loop/jmp G1 ct=$01 → $B22D
        .byte   $EE,$17,$06,$DC,$08,$00,$09,$03 ; --- Track $0A: Wily Fortress 3-4 ch1 (Pulse 2) ---
        .byte   $AA,$A8,$A6,$A5,$04,$00,$04,$00 ; cmd: NOP $00
        .byte   $8A,$0E,$1D,$B2,$66,$88,$88,$04 ; cmd: loop/jmp G0 ct=$1D → $B266
        .byte   $00,$86,$0E,$17,$B2,$6F,$04,$00 ; cmd: loop/jmp G0 ct=$17 → $B26F
        .byte   $85,$0E,$07,$B2,$76,$04,$00,$04 ; cmd: loop/jmp G0 ct=$07 → $B276
        .byte   $00,$8A,$0E,$0F,$B2,$7F,$04,$00 ; cmd: loop/jmp G0 ct=$0F → $B27F
        .byte   $88,$0E,$0F,$B2,$86,$04,$00,$86 ; cmd: loop/jmp G0 ct=$0F → $B286
        .byte   $0E,$0F,$B2,$8D,$04,$00,$85,$0E ; cmd: loop/jmp G0 ct=$0F → $B28D
        .byte   $0F,$B2,$94,$0F,$01,$B2,$7D,$16 ; cmd: loop/jmp G1 ct=$01 → $B27D
        .byte   $B2,$64,$17,$06,$C8,$07,$0A,$08 ; --- Track $0A: Wily Fortress 3-4 ch0 (Triangle) ---
        .byte   $0C,$04,$00,$6C,$60,$6C,$6C,$0E ; cmd: NOP $00
        .byte   $03,$B2,$A9,$04,$00,$04,$00,$85 ; cmd: NOP $00
        .byte   $8C,$88,$8C,$65,$6D,$6F,$6D,$6A ; note data
        .byte   $6D,$6F,$6D,$0E,$17,$B2,$B5,$16 ; cmd: loop/jmp G0 ct=$17 → $B2B5
; --- Track $0B: Wily Fortress 5-6 ($B2CB) ---
        .byte   $B2,$B3,$17,$00,$B2,$D4,$B3,$29 ; Track $0B header: $00=music (Wily Fortress 5-6)
        .byte   $B3,$87,$B3,$D2,$05,$02,$00,$06 ; ch1 (Pulse 2) ptr hi
        .byte   $96,$07,$0C,$08,$12,$18,$40,$04 ; cmd: set octave 4
        .byte   $08,$89,$8A,$8C,$8D,$0E,$05,$B2 ; cmd: loop/jmp G0 ct=$05 → $B2DF
        .byte   $DF,$8D,$8C,$8D,$8F,$90,$8F,$8D ; note data
        .byte   $8C,$04,$00,$09,$01,$04,$00,$04 ; cmd: NOP $00
        .byte   $00,$07,$0A,$99,$9B,$9C,$9E,$0E ; cmd: set octave 2
        .byte   $0D,$B2,$F7,$99,$98,$99,$9B,$9C ; note data
        .byte   $9B,$99,$98,$0F,$01,$B2,$F5,$04 ; cmd: loop/jmp G1 ct=$01 → $B2F5
        .byte   $00,$07,$0C,$99,$98,$99,$80,$0E ; cmd: set octave 4
        .byte   $0D,$B3,$0F,$04,$00,$9C,$9B,$99 ; cmd: NOP $00
        .byte   $98,$0E,$01,$B3,$1B,$16,$B2,$F1 ; cmd: loop/jmp G0 ct=$01 → $B31B
        .byte   $17,$06,$96,$07,$0C,$08,$1A,$18 ; --- Track $0B: Wily Fortress 5-6 ch2 (Pulse 1) ---
        .byte   $40,$04,$00,$9E,$9F,$03,$88,$89 ; cmd: NOP $00
        .byte   $0E,$05,$B3,$31,$06,$FF,$E8,$04 ; cmd: loop/jmp G0 ct=$05 → $B331
        .byte   $08,$04,$08,$04,$08,$09,$01,$06 ; cmd: NOP $08
        .byte   $96,$A8,$A7,$A8,$A9,$0E,$03,$B3 ; cmd: loop/jmp G0 ct=$03 → $B343
        .byte   $43,$A3,$A2,$A3,$A4,$A3,$A2,$A3 ; note data
        .byte   $A4,$E1,$03,$F8,$0F,$01,$B3,$41 ; cmd: toggle bit 3
        .byte   $04,$08,$09,$00,$08,$1C,$CD,$CC ; cmd: NOP $08
        .byte   $CB,$CA,$C9,$CB,$13,$08,$B3,$7B ; cmd: loop end G1 fl=$08 → $B37B
        .byte   $04,$08,$90,$0E,$07,$B3,$70,$0F ; cmd: NOP $08
        .byte   $01,$B3,$60,$8D,$89,$88,$84,$8D ; note data w/ rests
        .byte   $89,$88,$84,$16,$B3,$3F,$17,$06 ; --- Track $0B: Wily Fortress 5-6 ch1 (Pulse 2) ---
        .byte   $FA,$08,$00,$09,$03,$ED,$E9,$EF ; cmd: set pitch ofs $00
        .byte   $E8,$04,$00,$06,$E6,$CD,$CD,$CD ; cmd: NOP $00
        .byte   $CD,$C9,$C9,$C9,$C9,$C6,$C6,$C6 ; note data
        .byte   $C6,$C8,$C8,$C8,$C8,$04,$00,$AD ; cmd: NOP $00
        .byte   $0E,$07,$B3,$A5,$04,$00,$A9,$0E ; cmd: loop/jmp G0 ct=$07 → $B3A5
        .byte   $07,$B3,$AC,$04,$00,$A6,$0E,$07 ; cmd: NOP $00
        .byte   $B3,$B3,$04,$00,$A8,$0E,$07,$B3 ; cmd: NOP $00
        .byte   $BA,$ED,$E6,$C9,$CB,$ED,$ED,$E6 ; note data
        .byte   $C9,$CB,$AD,$AD,$AD,$AD,$16,$B3 ; cmd: end channel
        .byte   $91,$17,$06,$FF,$07,$0C,$08,$0C ; --- Track $0B: Wily Fortress 5-6 ch0 (Triangle) ---
        .byte   $04,$00,$06,$64,$8C,$80,$8C,$8C ; cmd: NOP $00
        .byte   $00,$6D,$6D,$6D,$00,$8C,$8C,$8C ; cmd: toggle legato
        .byte   $0E,$03,$B3,$D8,$04,$00,$06,$FF ; cmd: loop/jmp G0 ct=$03 → $B3D8
        .byte   $04,$00,$85,$85,$88,$85,$8A,$8C ; cmd: NOP $00
        .byte   $8D,$8F,$0E,$0F,$B3,$F0,$04,$00 ; cmd: loop/jmp G0 ct=$0F → $B3F0
        .byte   $06,$64,$8C,$00,$6D,$6D,$6D,$00 ; cmd: set dur mul $64
        .byte   $8C,$8C,$8F,$8C,$8C,$8C,$0E,$07 ; cmd: loop/jmp G0 ct=$07 → $B3FE
; --- Track $0C: Wily Fortress Map ($B416) ---
        .byte   $B3,$FE,$16,$B3,$EC,$17,$00,$B4 ; Track $0C header: $00=music (Wily Fortress Map)
        .byte   $1F,$B4,$F6,$B5,$59,$B5,$A5,$05 ; ch3 ptr lo → $B41F
        .byte   $02,$2E,$0A,$03,$06,$A0,$07,$0A ; cmd: loop start G0 ct=$03
        .byte   $18,$80,$08,$16,$09,$01,$04,$08 ; cmd: set duty 50%
        .byte   $6D,$6E,$6D,$6C,$0E,$0F,$B4,$2E ; cmd: loop/jmp G0 ct=$0F → $B42E
        .byte   $04,$08,$04,$08,$07,$0C,$08,$16 ; cmd: NOP $08
        .byte   $18,$40,$06,$C8,$6C,$6C,$60,$6D ; cmd: set duty 25%
        .byte   $6D,$60,$6E,$6E,$60,$6D,$6D,$60 ; note data w/ rests
        .byte   $06,$C8,$6C,$6D,$6E,$60,$0E,$01 ; cmd: set dur mul $C8
        .byte   $B4,$3A,$04,$48,$07,$08,$06,$FF ; cmd: NOP $48
        .byte   $18,$80,$08,$11,$A8,$08,$05,$01 ; cmd: set duty 50%
        .byte   $C8,$08,$11,$81,$01,$87,$A7,$08 ; cmd: set pitch ofs $11
        .byte   $05,$01,$C7,$08,$11,$88,$87,$01 ; cmd: toggle bit 6
        .byte   $A6,$08,$05,$01,$C6,$08,$11,$81 ; cmd: set pitch ofs $05
        .byte   $01,$84,$A4,$08,$05,$02,$01,$C4 ; cmd: toggle bit 6
        .byte   $0E,$01,$B4,$5A,$08,$11,$01,$CD ; cmd: loop/jmp G0 ct=$01 → $B45A
        .byte   $08,$05,$01,$AD,$08,$11,$86,$01 ; cmd: set pitch ofs $05
        .byte   $8B,$AB,$08,$05,$CB,$08,$11,$01 ; cmd: set pitch ofs $05
        .byte   $8B,$08,$00,$6D,$6B,$08,$11,$01 ; cmd: set pitch ofs $00
        .byte   $A9,$08,$05,$C9,$06,$96,$08,$00 ; cmd: set pitch ofs $05
        .byte   $01,$89,$06,$FF,$08,$00,$69,$68 ; cmd: toggle bit 6
        .byte   $08,$11,$A5,$A6,$A8,$A9,$01,$AD ; cmd: set pitch ofs $11
        .byte   $08,$05,$01,$CD,$86,$08,$11,$01 ; cmd: set pitch ofs $05
        .byte   $8E,$AE,$08,$05,$CE,$06,$96,$08 ; cmd: set pitch ofs $05
        .byte   $00,$01,$8E,$06,$FF,$08,$00,$6E ; cmd: toggle bit 6
        .byte   $6D,$08,$11,$01,$AB,$08,$05,$CB ; cmd: set pitch ofs $11
        .byte   $06,$96,$08,$00,$01,$8B,$06,$FF ; cmd: set dur mul $96
        .byte   $08,$00,$6B,$6D,$08,$11,$AE,$B1 ; cmd: set pitch ofs $00
        .byte   $B2,$B4,$16,$B4,$38,$17,$06,$FF ; --- Track $0C: Wily Fortress Map ch2 (Pulse 1) ---
        .byte   $07,$0C,$18,$40,$09,$02,$08,$18 ; cmd: set octave 4
        .byte   $ED,$F4,$F3,$EF,$04,$08,$04,$08 ; cmd: NOP $08
        .byte   $06,$C8,$18,$80,$08,$16,$09,$00 ; cmd: set dur mul $C8
        .byte   $07,$0C,$72,$72,$60,$73,$73,$60 ; cmd: set octave 4
        .byte   $74,$74,$60,$73,$73,$60,$06,$C8 ; cmd: set dur mul $C8
        .byte   $72,$73,$74,$60,$0E,$01,$B5,$06 ; cmd: loop/jmp G0 ct=$01 → $B506
        .byte   $04,$00,$18,$C0,$08,$16,$07,$0A ; cmd: NOP $00
        .byte   $80,$99,$7B,$60,$74,$60,$9B,$7C ; note data w/ rests
        .byte   $60,$79,$60,$74,$60,$0E,$07,$B5 ; cmd: loop/jmp G0 ct=$07 → $B528
        .byte   $28,$04,$00,$80,$9E,$03,$68,$60 ; cmd: NOP $00
        .byte   $61,$60,$88,$69,$60,$66,$60,$61 ; note data w/ rests
        .byte   $60,$0E,$07,$B5,$41,$16,$B5,$04 ; cmd: loop/jmp G0 ct=$07 → $B541
        .byte   $17,$06,$FF,$08,$02,$09,$02,$ED ; --- Track $0C: Wily Fortress Map ch1 (Pulse 2) ---
        .byte   $F4,$F3,$EF,$04,$00,$08,$02,$06 ; cmd: NOP $00
        .byte   $B4,$09,$03,$04,$00,$61,$61,$60 ; cmd: set detune $03
        .byte   $62,$62,$60,$63,$63,$60,$62,$62 ; note data w/ rests
        .byte   $60,$61,$62,$63,$60,$0E,$01,$B5 ; cmd: loop/jmp G0 ct=$01 → $B56B
        .byte   $6B,$06,$C8,$09,$02,$04,$00,$8D ; cmd: set dur mul $C8
        .byte   $8D,$8B,$8D,$80,$8D,$8B,$8D,$0E ; cmd: loop/jmp G0 ct=$07 → $B585
        .byte   $07,$B5,$85,$04,$00,$92,$92,$90 ; cmd: NOP $00
        .byte   $92,$80,$92,$90,$92,$0E,$07,$B5 ; cmd: loop/jmp G0 ct=$07 → $B593
        .byte   $93,$16,$B5,$63,$17,$06,$FF,$07 ; --- Track $0C: Wily Fortress Map ch0 (Triangle) ---
        .byte   $0C,$08,$19,$C8,$C0,$E0,$04,$00 ; cmd: set pitch ofs $19
        .byte   $08,$0C,$06,$C8,$A5,$0E,$05,$B5 ; cmd: set pitch ofs $0C
        .byte   $AE,$04,$00,$68,$0E,$07,$B5,$B9 ; cmd: NOP $00
        .byte   $04,$00,$07,$0B,$06,$FF,$04,$00 ; cmd: NOP $00
        .byte   $04,$00,$68,$68,$60,$0E,$03,$B5 ; cmd: NOP $00
        .byte   $C8,$06,$FF,$68,$68,$68,$60,$0F ; cmd: set dur mul $FF
        .byte   $01,$B5,$C6,$06,$C8,$04,$00,$65 ; cmd: set dur mul $C8
        .byte   $60,$65,$6E,$68,$65,$6E,$65,$65 ; note data w/ rests
        .byte   $60,$65,$60,$68,$65,$8E,$0E,$0F ; cmd: loop/jmp G0 ct=$0F → $B5DD
; --- Track $0D: Boss Battle ($B5F6) ---
        .byte   $B5,$DD,$16,$B5,$C0,$17,$00,$B5 ; Track $0D header: $00=music (Boss Battle)
        .byte   $FF,$B6,$CC,$B7,$89,$B7,$E5,$05 ; ch3 ptr lo → $B5FF
        .byte   $02,$66,$06,$C8,$07,$0C,$09,$01 ; cmd: set dur mul $C8
        .byte   $18,$80,$08,$1C,$03,$6E,$6B,$68 ; cmd: set duty 50%
        .byte   $65,$6B,$68,$65,$62,$68,$65,$62 ; note data
        .byte   $03,$77,$7D,$7A,$77,$74,$7A,$77 ; cmd: toggle bit 3
        .byte   $74,$71,$77,$74,$71,$6E,$74,$71 ; note data
        .byte   $6E,$6B,$71,$6E,$6B,$68,$04,$00 ; cmd: NOP $00
        .byte   $18,$40,$08,$0B,$90,$90,$08,$16 ; cmd: set duty 25%
        .byte   $9F,$01,$03,$C9,$01,$89,$0E,$01 ; cmd: toggle bit 6
        .byte   $B6,$2E,$05,$02,$49,$04,$00,$04 ; cmd: set tempo $02,$49
        .byte   $00,$09,$02,$06,$FF,$07,$0E,$08 ; cmd: set detune $02
        .byte   $23,$18,$C0,$90,$94,$95,$8F,$80 ; cmd: set duty 75%
        .byte   $8E,$80,$8C,$80,$8B,$80,$88,$C9 ; note data w/ rests
        .byte   $0E,$01,$B6,$47,$07,$0C,$08,$1D ; cmd: loop/jmp G0 ct=$01 → $B647
        .byte   $06,$C8,$02,$AC,$6B,$6C,$02,$AE ; cmd: set dur mul $C8
        .byte   $6C,$6E,$02,$B0,$74,$75,$02,$B7 ; cmd: set dotted note
        .byte   $75,$77,$02,$B8,$77,$78,$02,$BA ; cmd: set dotted note
        .byte   $78,$7A,$02,$BC,$7A,$7C,$02,$BD ; cmd: set dotted note
        .byte   $7C,$7A,$06,$96,$7C,$7B,$7A,$79 ; cmd: set dur mul $96
        .byte   $78,$77,$76,$75,$74,$75,$77,$78 ; note data
        .byte   $7A,$7C,$7D,$03,$68,$6B,$6A,$69 ; cmd: toggle bit 3
        .byte   $68,$67,$66,$65,$64,$63,$64,$68 ; note data
        .byte   $69,$6B,$6C,$6E,$6F,$04,$00,$09 ; cmd: NOP $00
        .byte   $00,$06,$64,$7C,$60,$7C,$60,$03 ; cmd: set dur mul $64
        .byte   $70,$64,$60,$64,$60,$70,$60,$64 ; note data w/ rests
        .byte   $64,$60,$64,$60,$0E,$01,$B6,$AD ; cmd: loop/jmp G0 ct=$01 → $B6AD
        .byte   $16,$B6,$45,$17,$06,$C8,$07,$0C ; --- Track $0D: Boss Battle ch2 (Pulse 1) ---
        .byte   $18,$40,$08,$22,$09,$01,$7D,$03 ; cmd: set duty 25%
        .byte   $71,$65,$71,$62,$6E,$62,$6E,$03 ; cmd: toggle bit 3
        .byte   $77,$03,$6B,$03,$77,$03,$6B,$03 ; cmd: toggle bit 3
        .byte   $74,$03,$68,$03,$74,$03,$68,$03 ; cmd: toggle bit 3
        .byte   $71,$7D,$71,$7D,$6E,$7A,$6E,$7A ; note data
        .byte   $6B,$77,$6B,$77,$68,$74,$68,$74 ; note data
        .byte   $04,$00,$08,$0B,$95,$95,$08,$16 ; cmd: NOP $00
        .byte   $03,$8B,$12,$08,$B7,$16,$01,$CC ; cmd: toggle bit 3
        .byte   $01,$8C,$0E,$01,$B7,$00,$18,$C0 ; cmd: toggle bit 6
        .byte   $08,$1C,$8C,$00,$75,$73,$70,$6E ; cmd: set pitch ofs $1C
        .byte   $6C,$69,$67,$64,$62,$03,$78,$75 ; cmd: toggle bit 3
        .byte   $73,$04,$08,$04,$08,$18,$80,$07 ; cmd: NOP $08
        .byte   $09,$08,$1C,$90,$89,$8F,$89,$90 ; cmd: set pitch ofs $1C
        .byte   $89,$91,$89,$90,$89,$8F,$89,$90 ; note data
        .byte   $89,$8B,$89,$0E,$01,$B7,$2B,$04 ; cmd: loop/jmp G0 ct=$01 → $B72B
        .byte   $08,$09,$00,$78,$77,$76,$75,$74 ; cmd: set detune $00
        .byte   $75,$76,$77,$0E,$07,$B7,$47,$97 ; cmd: loop/jmp G0 ct=$07 → $B747
        .byte   $90,$95,$90,$97,$90,$98,$90,$97 ; note data
        .byte   $90,$95,$90,$97,$90,$94,$90,$04 ; cmd: NOP $08
        .byte   $08,$09,$01,$07,$0C,$18,$40,$08 ; cmd: set detune $01
        .byte   $16,$6B,$60,$6B,$60,$77,$6B,$60 ; note data w/ rests
        .byte   $6B,$60,$77,$60,$6B,$6B,$60,$6B ; note data w/ rests
        .byte   $60,$0E,$01,$B7,$67,$16,$B7,$29 ; cmd: loop/jmp G0 ct=$01 → $B767
        .byte   $17,$06,$DC,$08,$00,$09,$03,$E0 ; --- Track $0D: Boss Battle ch1 (Pulse 2) ---
        .byte   $E0,$89,$89,$02,$C0,$89,$89,$02 ; cmd: set dotted note
        .byte   $C0,$04,$00,$04,$00,$89,$0E,$1F ; cmd: NOP $00
        .byte   $B7,$9B,$04,$00,$87,$0E,$07,$B7 ; cmd: NOP $00
        .byte   $A2,$04,$00,$88,$0E,$07,$B7,$A9 ; cmd: NOP $00
        .byte   $89,$89,$89,$89,$8B,$8B,$8B,$8B ; note data
        .byte   $8C,$8C,$8C,$8C,$09,$02,$8E,$8E ; cmd: set detune $02
        .byte   $8E,$8E,$04,$00,$90,$0E,$0F,$B7 ; cmd: NOP $00
        .byte   $C2,$04,$00,$09,$03,$64,$60,$64 ; cmd: NOP $00
        .byte   $60,$70,$64,$60,$64,$60,$70,$60 ; note data w/ rests
        .byte   $64,$64,$60,$64,$60,$0E,$01,$B7 ; cmd: loop/jmp G0 ct=$01 → $B7C9
        .byte   $C9,$16,$B7,$99,$17,$07,$0C,$08 ; --- Track $0D: Boss Battle ch0 (Triangle) ---
        .byte   $0C,$E0,$E0,$06,$0A,$86,$86,$02 ; cmd: set dur mul $0A
        .byte   $C0,$86,$86,$02,$C0,$04,$00,$06 ; cmd: set dotted note
        .byte   $FF,$07,$0A,$04,$00,$63,$6E,$66 ; cmd: set octave 2
        .byte   $6E,$68,$6E,$66,$63,$60,$63,$63 ; note data w/ rests
        .byte   $66,$68,$6E,$66,$6E,$0E,$09,$B7 ; cmd: loop/jmp G0 ct=$09 → $B7FB
        .byte   $FB,$04,$00,$68,$60,$68,$60,$68 ; cmd: NOP $00
        .byte   $68,$60,$68,$60,$68,$60,$68,$68 ; note data w/ rests
        .byte   $60,$68,$60,$0E,$01,$B8,$11,$16 ; cmd: loop/jmp G0 ct=$01 → $B811
; --- Track $0E: Password Screen ($B82B) ---
        .byte   $B7,$F5,$17,$00,$B8,$34,$B8,$95 ; Track $0E header: $00=music (Password Screen)
        .byte   $B8,$E9,$B9,$1D,$0A,$05,$05,$02 ; ch1 (Pulse 2) ptr hi
        .byte   $00,$06,$50,$07,$0C,$08,$1C,$09 ; cmd: set dur mul $50
        .byte   $01,$18,$80,$04,$28,$04,$28,$B2 ; cmd: set duty 50%
        .byte   $89,$B2,$89,$B0,$89,$B0,$89,$AE ; note data
        .byte   $89,$AE,$89,$B0,$89,$B0,$89,$06 ; cmd: set dur mul $96
        .byte   $96,$12,$28,$B8,$77,$AE,$92,$95 ; cmd: loop end G0 fl=$28 → $B877
        .byte   $80,$97,$A0,$97,$A0,$01,$97,$00 ; cmd: toggle bit 6
        .byte   $01,$B7,$00,$B5,$92,$95,$8D,$93 ; cmd: toggle bit 6
        .byte   $92,$8D,$93,$0E,$01,$B8,$45,$B5 ; cmd: loop/jmp G0 ct=$01 → $B845
        .byte   $8E,$92,$80,$95,$A0,$8E,$92,$80 ; note data w/ rests
        .byte   $01,$95,$01,$B5,$00,$53,$55,$00 ; cmd: toggle bit 6
        .byte   $53,$B2,$90,$92,$80,$8D,$8E,$80 ; note data w/ rests
        .byte   $90,$16,$B8,$43,$17,$06,$50,$07 ; --- Track $0E: Password Screen ch2 (Pulse 1) ---
        .byte   $0B,$08,$1C,$09,$01,$18,$40,$04 ; cmd: set pitch ofs $1C
        .byte   $28,$04,$28,$AE,$80,$AE,$80,$AD ; cmd: NOP $28
        .byte   $80,$AD,$80,$A9,$80,$A9,$80,$AD ; note data w/ rests
        .byte   $80,$AD,$80,$06,$96,$12,$28,$B8 ; cmd: set dur mul $96
        .byte   $D2,$A9,$8E,$B2,$8E,$A0,$8E,$A0 ; note data w/ rests
        .byte   $01,$8E,$00,$01,$AE,$00,$AE,$8E ; cmd: toggle bit 6
        .byte   $8D,$80,$8D,$8D,$80,$8D,$0E,$01 ; cmd: loop/jmp G0 ct=$01 → $B8A1
        .byte   $B8,$A1,$A9,$8E,$B2,$8E,$A0,$8E ; note data w/ rests
        .byte   $A0,$8E,$00,$A0,$00,$AE,$8E,$8D ; cmd: toggle legato
        .byte   $80,$8D,$8D,$80,$8D,$16,$B8,$9F ; cmd: end channel
        .byte   $17,$06,$C8,$08,$02,$07,$0C,$09 ; --- Track $0E: Password Screen ch1 (Pulse 2) ---
        .byte   $02,$04,$20,$04,$20,$A2,$80,$A2 ; cmd: NOP $20
        .byte   $80,$A6,$80,$A6,$80,$A7,$80,$A7 ; note data w/ rests
        .byte   $80,$A9,$80,$A9,$80,$AB,$80,$AB ; note data w/ rests
        .byte   $80,$A9,$80,$A9,$80,$A7,$80,$A7 ; note data w/ rests
        .byte   $80,$A9,$87,$A6,$84,$0E,$01,$B8 ; cmd: loop/jmp G0 ct=$01 → $B8F3
; --- Track $0F: Continue Screen ($B91E) ---
        .byte   $F3,$16,$B8,$F1,$17,$17,$00,$B9 ; Track $0F header: $00=music (Continue Screen)
        .byte   $27,$BA,$D2,$BC,$46,$BD,$9A,$05 ; ch3 ptr lo → $B927
        .byte   $01,$C7,$06,$FF,$07,$0F,$18,$40 ; cmd: set dur mul $FF
        .byte   $08,$16,$09,$00,$E0,$0A,$05,$03 ; cmd: set pitch ofs $16
        .byte   $B6,$00,$34,$32,$31,$2F,$2D,$2C ; cmd: toggle legato
        .byte   $2A,$2C,$28,$26,$25,$23,$21,$03 ; cmd: toggle bit 3
        .byte   $38,$36,$34,$32,$31,$2F,$2D,$A0 ; note data w/ rests
        .byte   $18,$C0,$06,$96,$08,$1C,$00,$03 ; cmd: set duty 75%
        .byte   $94,$96,$97,$04,$08,$09,$01,$8D ; cmd: NOP $08
        .byte   $8B,$6A,$88,$8D,$02,$8B,$6A,$88 ; cmd: set dotted note
        .byte   $01,$6A,$12,$48,$B9,$78,$CA,$01 ; cmd: toggle bit 6
        .byte   $8A,$88,$8A,$8B,$0E,$01,$B9,$5B ; cmd: loop/jmp G0 ct=$01 → $B95B
        .byte   $08,$16,$06,$64,$01,$8A,$02,$8A ; cmd: set pitch ofs $16
        .byte   $8A,$8A,$01,$AA,$01,$6A,$8B,$8A ; cmd: toggle bit 6
        .byte   $04,$00,$08,$1C,$09,$02,$07,$0F ; cmd: NOP $00
        .byte   $55,$02,$76,$74,$93,$94,$02,$93 ; cmd: set dotted note
        .byte   $71,$8F,$01,$71,$01,$D1,$80,$8C ; cmd: toggle bit 6
        .byte   $6F,$8C,$01,$73,$01,$B3,$B4,$93 ; cmd: toggle bit 6
        .byte   $91,$8F,$71,$01,$73,$D3,$01,$93 ; cmd: toggle bit 6
        .byte   $96,$93,$01,$8E,$01,$CE,$01,$AD ; cmd: toggle bit 6
        .byte   $01,$AD,$80,$8C,$6C,$8E,$01,$CF ; cmd: toggle bit 6
        .byte   $01,$6F,$80,$8C,$6C,$8E,$8F,$02 ; cmd: toggle bit 6
        .byte   $91,$93,$94,$55,$02,$01,$D6,$02 ; cmd: set dotted note
        .byte   $01,$76,$06,$FF,$08,$16,$99,$80 ; cmd: toggle bit 6
        .byte   $99,$B8,$96,$B4,$57,$02,$01,$78 ; cmd: set dotted note
        .byte   $01,$B8,$00,$56,$54,$53,$51,$01 ; cmd: toggle bit 6
        .byte   $6F,$02,$00,$01,$8F,$60,$06,$96 ; cmd: set dotted note
        .byte   $08,$1C,$8C,$8F,$91,$B3,$92,$B3 ; cmd: set pitch ofs $1C
        .byte   $B6,$01,$8F,$01,$CF,$93,$8F,$8C ; cmd: toggle bit 6
        .byte   $01,$93,$01,$F3,$A0,$AC,$8F,$B3 ; cmd: toggle bit 6
        .byte   $01,$94,$06,$64,$01,$94,$94,$B3 ; cmd: toggle bit 6
        .byte   $96,$B3,$01,$91,$02,$D1,$01,$91 ; cmd: toggle bit 6
        .byte   $01,$91,$01,$91,$93,$02,$B0,$9A ; cmd: toggle bit 6
        .byte   $98,$76,$01,$78,$D8,$01,$98,$09 ; cmd: toggle bit 6
        .byte   $01,$06,$64,$98,$9A,$9C,$02,$03 ; cmd: set dur mul $64
        .byte   $89,$02,$87,$02,$85,$02,$83,$62 ; cmd: set dotted note
        .byte   $60,$03,$78,$60,$01,$D7,$01,$97 ; cmd: toggle bit 3
        .byte   $7F,$60,$03,$69,$60,$03,$77,$01 ; cmd: toggle bit 3
        .byte   $79,$D9,$01,$99,$99,$9A,$9C,$02 ; cmd: toggle bit 6
        .byte   $9D,$02,$9B,$02,$03,$88,$02,$86 ; cmd: set dotted note
        .byte   $65,$60,$63,$60,$02,$85,$02,$83 ; cmd: set dotted note
        .byte   $02,$88,$02,$86,$65,$60,$63,$60 ; cmd: set dotted note
        .byte   $01,$C5,$01,$85,$85,$86,$01,$88 ; cmd: toggle bit 6
        .byte   $C8,$01,$88,$85,$86,$88,$01,$C9 ; cmd: toggle bit 6
        .byte   $01,$89,$89,$87,$85,$02,$84,$02 ; cmd: toggle bit 6
        .byte   $82,$61,$60,$02,$82,$02,$84,$65 ; cmd: set dotted note
        .byte   $60,$01,$CB,$01,$8B,$8B,$89,$8B ; cmd: toggle bit 6
        .byte   $02,$8B,$6C,$80,$01,$C9,$01,$89 ; cmd: set dotted note
        .byte   $02,$89,$02,$87,$02,$8C,$02,$8A ; cmd: set dotted note
        .byte   $69,$60,$67,$60,$02,$86,$02,$85 ; cmd: set dotted note
        .byte   $02,$88,$02,$86,$65,$60,$63,$60 ; cmd: set dotted note
        .byte   $0C,$03,$EA,$08,$00,$06,$B4,$07 ; cmd: loop start G2 ct=$03
        .byte   $0C,$02,$8A,$02,$8A,$6A,$6A,$02 ; cmd: set dotted note
        .byte   $8A,$02,$8A,$6A,$6A,$00,$56,$51 ; cmd: set dotted note
        .byte   $4D,$4A,$45,$41,$0C,$00,$16,$B9 ; cmd: loop start G2 ct=$00
        .byte   $88,$17,$06,$C8,$07,$06,$18,$40 ; --- Track $0F: Continue Screen ch2 (Pulse 1) ---
        .byte   $08,$11,$09,$00,$E0,$03,$EE,$F1 ; cmd: set pitch ofs $11
        .byte   $EE,$F1,$07,$09,$06,$64,$08,$16 ; cmd: set octave 1
        .byte   $8E,$02,$8E,$8E,$8E,$01,$AE,$01 ; cmd: set dotted note
        .byte   $6E,$8F,$06,$C8,$8E,$04,$00,$80 ; cmd: set dur mul $C8
        .byte   $02,$C0,$18,$40,$09,$01,$07,$06 ; cmd: set dotted note
        .byte   $08,$11,$06,$C8,$03,$C8,$AC,$B1 ; cmd: set pitch ofs $11
        .byte   $01,$CA,$01,$8A,$08,$16,$6E,$71 ; cmd: toggle bit 6
        .byte   $60,$02,$94,$08,$11,$D3,$D2,$D1 ; cmd: set dotted note
        .byte   $D0,$EF,$EE,$02,$01,$CD,$01,$8D ; cmd: set dotted note
        .byte   $08,$16,$01,$91,$01,$91,$91,$AF ; cmd: set pitch ofs $16
        .byte   $8F,$AD,$80,$08,$11,$EF,$18,$80 ; cmd: set pitch ofs $11
        .byte   $F3,$09,$01,$18,$40,$07,$0C,$06 ; cmd: set detune $01
        .byte   $B4,$08,$24,$98,$93,$8F,$98,$97 ; cmd: set pitch ofs $24
        .byte   $93,$8F,$01,$96,$01,$96,$93,$8F ; cmd: toggle bit 6
        .byte   $96,$95,$91,$8E,$08,$1C,$01,$8F ; cmd: set pitch ofs $1C
        .byte   $02,$CF,$01,$8F,$74,$00,$53,$51 ; cmd: set dotted note
        .byte   $4F,$00,$80,$07,$0A,$06,$64,$8F ; cmd: toggle legato
        .byte   $AF,$91,$B1,$01,$87,$02,$C7,$01 ; cmd: toggle bit 6
        .byte   $87,$01,$87,$01,$87,$87,$A7,$09 ; cmd: toggle bit 6
        .byte   $00,$06,$C8,$08,$16,$78,$76,$75 ; cmd: set dur mul $C8
        .byte   $76,$78,$09,$01,$6E,$70,$71,$03 ; cmd: set detune $01
        .byte   $98,$78,$78,$78,$98,$78,$C0,$07 ; cmd: set octave 5
        .byte   $05,$08,$11,$B1,$B5,$B8,$BB,$04 ; cmd: set pitch ofs $11
        .byte   $00,$07,$0A,$06,$96,$08,$16,$90 ; cmd: set octave 2
        .byte   $03,$90,$6E,$90,$60,$90,$6E,$01 ; cmd: toggle bit 3
        .byte   $70,$01,$B0,$0E,$01,$BB,$8F,$04 ; cmd: toggle bit 6
        .byte   $00,$09,$01,$80,$03,$8D,$6C,$8D ; cmd: set detune $01
        .byte   $60,$8D,$6C,$8D,$60,$09,$02,$6D ; cmd: set detune $02
        .byte   $6D,$0E,$01,$BB,$A7,$06,$FF,$03 ; cmd: loop/jmp G0 ct=$01 → $BBA7
        .byte   $58,$02,$99,$02,$98,$01,$D4,$02 ; cmd: set dotted note
        .byte   $01,$74,$18,$00,$07,$0D,$08,$00 ; cmd: toggle bit 6
        .byte   $80,$03,$68,$80,$68,$80,$68,$07 ; cmd: toggle bit 3
        .byte   $0A,$09,$01,$6A,$6C,$6D,$6F,$70 ; cmd: set detune $01
        .byte   $71,$72,$07,$08,$08,$11,$01,$F3 ; cmd: set octave 0
        .byte   $01,$B3,$B1,$B0,$AE,$04,$08,$18 ; cmd: toggle bit 6
        .byte   $40,$06,$96,$08,$1C,$07,$0A,$8B ; cmd: set dur mul $96
        .byte   $77,$75,$6B,$67,$18,$80,$08,$20 ; cmd: set duty 50%
        .byte   $8C,$18,$40,$08,$1C,$77,$75,$6B ; cmd: set duty 25%
        .byte   $67,$18,$80,$08,$20,$AE,$0E,$01 ; cmd: set duty 50%
        .byte   $BB,$ED,$07,$06,$18,$40,$08,$11 ; cmd: set octave 6
        .byte   $09,$01,$02,$B5,$02,$B3,$AE,$02 ; cmd: set detune $01
        .byte   $B4,$02,$B2,$AD,$07,$06,$F6,$18 ; cmd: set dotted note
        .byte   $00,$08,$00,$07,$0C,$06,$B4,$02 ; cmd: set pitch ofs $00
        .byte   $96,$02,$96,$76,$76,$02,$96,$02 ; cmd: set dotted note
        .byte   $96,$76,$76,$00,$56,$51,$4D,$4A ; cmd: toggle legato
        .byte   $45,$41,$16,$BA,$F5,$17,$06,$E6 ; --- Track $0F: Continue Screen ch1 (Pulse 2) ---
        .byte   $08,$0E,$09,$02,$03,$6A,$6A,$86 ; cmd: set pitch ofs $0E
        .byte   $6A,$6A,$86,$60,$6A,$60,$6A,$00 ; cmd: toggle legato
        .byte   $6A,$6A,$6A,$00,$86,$08,$02,$06 ; cmd: toggle legato
        .byte   $78,$09,$03,$04,$00,$8A,$8A,$65 ; cmd: set detune $03
        .byte   $88,$8A,$8A,$65,$88,$8A,$12,$00 ; cmd: loop end G0 fl=$00 → $BC7F
        .byte   $BC,$7F,$8B,$8B,$65,$88,$8B,$8B ; note data
        .byte   $65,$88,$8B,$0E,$02,$BC,$63,$8A ; cmd: loop/jmp G0 ct=$02 → $BC63
        .byte   $04,$00,$80,$02,$C0,$06,$C8,$A5 ; cmd: NOP $00
        .byte   $09,$02,$AC,$B1,$AC,$AA,$B1,$AA ; cmd: set detune $02
        .byte   $B1,$B3,$8E,$93,$B2,$8E,$92,$B1 ; note data
        .byte   $8E,$91,$B0,$AC,$B4,$AF,$B4,$8F ; note data
        .byte   $94,$B6,$B1,$B6,$98,$9A,$BB,$B6 ; note data
        .byte   $BB,$96,$99,$80,$99,$B8,$96,$B6 ; note data w/ rests
        .byte   $94,$B4,$AF,$B4,$AF,$B5,$90,$B5 ; note data
        .byte   $B5,$01,$98,$01,$98,$98,$98,$B7 ; cmd: toggle bit 6
        .byte   $B7,$01,$96,$01,$96,$93,$8F,$B5 ; cmd: toggle bit 6
        .byte   $91,$AE,$B4,$AF,$94,$AF,$95,$80 ; note data w/ rests
        .byte   $95,$B5,$96,$B3,$91,$09,$01,$80 ; cmd: set detune $01
        .byte   $98,$98,$98,$98,$B8,$09,$02,$8C ; cmd: set detune $02
        .byte   $80,$8C,$01,$AC,$01,$AC,$B2,$78 ; cmd: toggle bit 6
        .byte   $78,$91,$75,$60,$95,$78,$60,$98 ; note data w/ rests
        .byte   $7C,$60,$9C,$78,$7B,$91,$75,$60 ; note data w/ rests
        .byte   $95,$78,$60,$98,$7B,$60,$9B,$77 ; note data w/ rests
        .byte   $7A,$90,$73,$60,$93,$77,$60,$97 ; note data w/ rests
        .byte   $7A,$60,$9A,$90,$60,$70,$60,$70 ; note data w/ rests
        .byte   $73,$76,$99,$98,$96,$93,$72,$6F ; note data
        .byte   $92,$76,$60,$96,$79,$60,$99,$6F ; note data w/ rests
        .byte   $60,$8F,$74,$78,$94,$6F,$60,$8F ; note data w/ rests
        .byte   $72,$60,$92,$74,$60,$94,$74,$78 ; note data w/ rests
        .byte   $8D,$71,$60,$91,$74,$60,$94,$78 ; note data w/ rests
        .byte   $60,$98,$02,$8D,$6D,$60,$6D,$78 ; cmd: set dotted note
        .byte   $79,$6D,$79,$6D,$71,$72,$74,$76 ; note data
        .byte   $78,$02,$8E,$75,$02,$95,$6E,$02 ; cmd: set dotted note
        .byte   $8E,$75,$95,$8E,$02,$93,$7A,$02 ; cmd: set dotted note
        .byte   $9A,$73,$02,$93,$7A,$9A,$93,$02 ; cmd: set dotted note
        .byte   $90,$77,$02,$97,$70,$02,$90,$77 ; cmd: set dotted note
        .byte   $97,$90,$02,$95,$7C,$02,$9C,$75 ; cmd: set dotted note
        .byte   $9D,$9C,$9A,$95,$02,$8F,$02,$94 ; cmd: set dotted note
        .byte   $02,$BA,$AF,$02,$92,$02,$99,$02 ; cmd: set dotted note
        .byte   $BE,$B9,$02,$8B,$02,$92,$02,$B7 ; cmd: set dotted note
        .byte   $B2,$97,$60,$97,$60,$77,$77,$97 ; note data w/ rests
        .byte   $60,$97,$60,$77,$77,$97,$16,$BC ; cmd: end channel
        .byte   $80,$17,$08,$0C,$07,$0C,$06,$96 ; --- Track $0F: Continue Screen ch0 (Triangle) ---
        .byte   $6C,$68,$65,$68,$6C,$68,$65,$68 ; note data
        .byte   $63,$68,$63,$68,$6C,$68,$85,$04 ; cmd: NOP $00
        .byte   $00,$68,$6F,$68,$6F,$63,$63,$6F ; note data
        .byte   $68,$6F,$68,$6F,$63,$63,$60,$63 ; note data w/ rests
        .byte   $60,$0E,$03,$BD,$AF,$68,$60,$68 ; cmd: loop/jmp G0 ct=$03 → $BDAF
        .byte   $60,$68,$68,$60,$68,$60,$02,$88 ; cmd: set dotted note
        .byte   $68,$60,$68,$60,$68,$60,$04,$00 ; cmd: NOP $00
        .byte   $06,$96,$80,$C0,$6F,$6F,$8D,$04 ; cmd: set dur mul $96
        .byte   $00,$63,$6F,$6D,$6F,$68,$63,$60 ; note data w/ rests
        .byte   $63,$60,$63,$60,$63,$68,$68,$12 ; cmd: loop end G0 fl=$00 → $BDF8
        .byte   $00,$BD,$F8,$8F,$0E,$06,$BD,$DF ; cmd: loop/jmp G0 ct=$06 → $BDDF
        .byte   $88,$80,$88,$A8,$88,$88,$80,$88 ; note data w/ rests
        .byte   $04,$00,$63,$6F,$6D,$6F,$68,$63 ; cmd: NOP $00
        .byte   $60,$63,$60,$63,$60,$63,$68,$68 ; note data w/ rests
        .byte   $8F,$0E,$01,$BE,$00,$04,$00,$63 ; cmd: loop/jmp G0 ct=$01 → $BE00
        .byte   $6F,$6D,$6F,$68,$6F,$6D,$6F,$63 ; note data
        .byte   $6F,$6D,$6F,$68,$6F,$6D,$6F,$0E ; cmd: loop/jmp G0 ct=$01 → $BE15
        .byte   $01,$BE,$15,$63,$6F,$6D,$6F,$68 ; note data
        .byte   $63,$60,$63,$60,$63,$60,$63,$68 ; note data w/ rests
        .byte   $68,$8F,$80,$88,$A8,$88,$A8,$88 ; note data w/ rests
        .byte   $80,$88,$88,$88,$88,$88,$80,$88 ; note data w/ rests
        .byte   $80,$88,$A8,$68,$68,$06,$FF,$8D ; cmd: set dur mul $FF
        .byte   $06,$96,$68,$68,$06,$FF,$8D,$06 ; cmd: set dur mul $96
        .byte   $96,$04,$00,$06,$96,$63,$6F,$6D ; cmd: NOP $00
        .byte   $6F,$68,$6F,$6D,$6F,$63,$6F,$6D ; note data
        .byte   $6F,$68,$6F,$6D,$6F,$12,$00,$BE ; cmd: loop end G0 fl=$00 → $BE86
        .byte   $86,$63,$6F,$6D,$6F,$68,$6F,$6D ; note data
        .byte   $6F,$63,$6A,$6F,$63,$68,$6F,$06 ; cmd: set dur mul $FF
        .byte   $FF,$8D,$0E,$07,$BE,$59,$02,$88 ; cmd: loop/jmp G0 ct=$07 → $BE59
        .byte   $02,$88,$68,$68,$02,$88,$02,$88 ; cmd: set dotted note
; --- Track $10: Stage Select ($BE97) ---
        .byte   $68,$68,$88,$16,$BD,$D6,$17,$00 ; Track $10 header: $00=music (Stage Select)
        .byte   $BE,$A0,$BE,$EF,$BF,$25,$BF,$AD ; ch3 (Noise/DPCM) ptr hi
        .byte   $0A,$03,$05,$01,$55,$06,$AA,$07 ; --- Track $10: Stage Select ch3 (Noise/DPCM) ---
        .byte   $0C,$08,$06,$09,$01,$18,$40,$04 ; cmd: set pitch ofs $06
        .byte   $00,$04,$00,$7E,$7E,$7C,$7E,$60 ; cmd: NOP $00
        .byte   $03,$69,$60,$68,$60,$64,$60,$66 ; cmd: toggle bit 3
        .byte   $12,$08,$BE,$CB,$86,$61,$64,$0E ; cmd: loop end G0 fl=$08 → $BECB
        .byte   $01,$BE,$B1,$00,$86,$89,$8B,$02 ; cmd: toggle legato
        .byte   $00,$8D,$4B,$49,$01,$A6,$01,$86 ; cmd: toggle legato
        .byte   $6D,$75,$74,$70,$60,$02,$92,$6D ; cmd: set dotted note
        .byte   $72,$70,$6B,$60,$AD,$46,$48,$69 ; note data w/ rests
        .byte   $68,$66,$64,$16,$BE,$AF,$17,$06 ; --- Track $10: Stage Select ch2 (Pulse 1) ---
        .byte   $AA,$07,$0C,$08,$06,$04,$00,$04 ; cmd: set octave 4
        .byte   $00,$09,$00,$0C,$00,$7E,$7E,$03 ; cmd: set detune $00
        .byte   $70,$66,$6D,$66,$6B,$66,$60,$12 ; cmd: loop end G0 fl=$08 → $BF16
        .byte   $08,$BF,$16,$66,$70,$66,$6D,$66 ; note data
        .byte   $6B,$66,$0E,$03,$BE,$F7,$66,$68 ; cmd: loop/jmp G0 ct=$03 → $BEF7
        .byte   $66,$0C,$FE,$09,$01,$69,$68,$66 ; cmd: loop start G2 ct=$FE
        .byte   $64,$16,$BE,$F5,$17,$06,$C8,$08 ; --- Track $10: Stage Select ch1 (Pulse 2) ---
        .byte   $00,$09,$03,$04,$00,$04,$00,$66 ; cmd: set detune $03
        .byte   $66,$08,$0E,$06,$E6,$72,$08,$00 ; cmd: set pitch ofs $0E
        .byte   $06,$C8,$66,$60,$66,$08,$0E,$06 ; cmd: set dur mul $C8
        .byte   $E6,$72,$08,$00,$06,$C8,$66,$60 ; cmd: set pitch ofs $00
        .byte   $66,$08,$0E,$06,$E6,$72,$08,$00 ; cmd: set pitch ofs $0E
        .byte   $06,$C8,$66,$66,$66,$08,$0E,$06 ; cmd: set dur mul $C8
        .byte   $E6,$72,$08,$00,$06,$C8,$66,$0E ; cmd: set pitch ofs $00
        .byte   $01,$BF,$2D,$04,$00,$62,$62,$08 ; cmd: NOP $00
        .byte   $0E,$06,$E6,$72,$08,$00,$06,$C8 ; cmd: set dur mul $E6
        .byte   $62,$60,$62,$08,$0E,$06,$E6,$72 ; cmd: set pitch ofs $0E
        .byte   $08,$00,$06,$C8,$62,$60,$62,$08 ; cmd: set pitch ofs $00
        .byte   $0E,$06,$E6,$72,$08,$00,$06,$C8 ; cmd: set dur mul $E6
        .byte   $62,$12,$00,$BF,$9D,$62,$62,$08 ; cmd: loop end G0 fl=$00 → $BF9D
        .byte   $0E,$06,$E6,$72,$08,$00,$06,$C8 ; cmd: set dur mul $E6
        .byte   $62,$0E,$01,$BF,$63,$64,$64,$08 ; cmd: loop/jmp G0 ct=$01 → $BF63
        .byte   $0E,$06,$E6,$72,$08,$00,$06,$C8 ; cmd: set dur mul $E6
        .byte   $64,$16,$BF,$2B,$17,$06,$C8,$07 ; --- Track $10: Stage Select ch0 (Triangle) ---
        .byte   $0A,$08,$0C,$04,$00,$04,$00,$66 ; cmd: set pitch ofs $0C
        .byte   $66,$6C,$66,$60,$66,$6C,$66,$60 ; note data w/ rests
        .byte   $66,$6C,$66,$66,$66,$6C,$66,$0E ; cmd: loop/jmp G0 ct=$03 → $BFB5
; --- Track $11: Proto Man Whistle ($BFCF) ---
        .byte   $03,$BF,$B5,$16,$BF,$B3,$17,$00 ; Track $11 header: $00=music (Proto Man Whistle)
        .byte   $BF,$D8,$C0,$1A,$C0,$1B,$C0,$1C ; ch3 (Noise/DPCM) ptr hi
        .byte   $0A,$FE,$05,$01,$11,$06,$C8,$07 ; --- Track $11: Proto Man Whistle ch3 (Noise/DPCM) ---
        .byte   $0C,$08,$26,$18,$80,$09,$03,$00 ; cmd: set pitch ofs $26
        .byte   $03,$8A,$8D,$04,$48,$2D,$0D,$28 ; cmd: toggle bit 3
        .byte   $2F,$4F,$02,$8F,$00,$01,$AF,$8D ; cmd: set dotted note
        .byte   $D1,$0D,$00,$8A,$8D,$01,$2D,$0D ; cmd: loop start G3 ct=$00
